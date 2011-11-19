;; balance-mode.el

;; Balance -- Major mode for recording transactions and balancing a bank
;;            account.  Based on balance-mode by Jason Baietto 
;;            (jason@ssd.csd.harris.com) and Bob Newell
;;
;; $Date: 2007/03/24 23:23:52 $
;; $Revision: 1.25 $
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 675
;; Mass Ave, Cambridge, MA 02139, USA.
;;
;; Put this file in your emacs load path and add the following lines to your
;; .emacs file:
;;
;;    (autoload 'balance-mode "balance")
;;    (setq auto-mode-alist
;;       (append '(("\\.bal$" . balance-mode)) auto-mode-alist))
;;
;; Then, doing a find-file on "sample.bal" will automatically load the balance
;; elisp and enter Balance major mode for the buffer visiting the file.
;; See the balance-mode mode help for details.
;; Balance will, if requested, remove a lot of old transactions and put it in 
;; a backup file.  To automatically open a backup file in balance-mode, put
;;
;;    (autoload 'balance-mode "balance")
;;    (setq auto-mode-alist
;;       (append '(("\\.bal\\.[0-9]+$" . balance-mode)) auto-mode-alist))
;; in your .emacs file.


(require 'calendar)

;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar balance-check-type "check[ \t]+\\([0-9]+\\)"
   "Type field for check transactions.  This is defined separately from
the other transaction types because it is used by functions that perform
special operations on check transactions.")

(defvar balance-transaction-types
   (list
      "check "
      "atm"                ;; Automatic teller machine.
      "bank"               ;; Initiated by the bank (fees, interest).
      "deposit"            ;; Non-direct deposits
      "teller"             ;; Human at teller or drive through.
      "direct"             ;; Direct deposit or direct withdrawl.
      "phone"              ;; Initiated over a touch-tone phone.
      "online"             ;; On-line banking transaction.
      "pos"                ;; Point of sale purchase (debit card).
      "misc"
      "")                  ;; This allows blank type fields.
   "List of valid expressions allowed in the transaction type field.
Anything else is an error and will be flagged during recalculation.")

(defvar balance-header
"Date           Type        Description                    Amount   Balance
==========================================================================")

(defvar balance-footer
"==========================================================================")

(defvar balance-date-column 0
   "Column where transaction date begins.")

(defvar balance-date-width 12
  "Width of date column.")

(defvar balance-clear-column (+ balance-date-column balance-date-width 1)
  "Column where status appears.")

(defvar balance-clear-width 1
  "Width of status column.")

(defvar balance-type-column (+ balance-clear-column balance-clear-width 1)
  "Column where transaction type begins.")

(defvar balance-type-width 11
  "Width of type column.")

(defvar balance-description-column (+ balance-type-column balance-type-width 1)
  "Column where transaction description begins.")

(defvar balance-description-width 25
  "Width of description column.")

(defvar balance-sign-column (+ balance-description-column balance-description-width 1)
  "Column where transaction sign begins.")

(defvar balance-sign-width 1
  "Width of sign column.")

(defvar balance-amount-column (+ balance-sign-column balance-sign-width 1)
  "Column where transaction amount begins.")

(defvar balance-amount-width 9
  "Width of amount column.")

(defvar balance-current-balance-column (+ balance-amount-column balance-amount-width 1)
  "Column where current balance begins.")

(defvar balance-tab-stop-list
   (list
      balance-date-column
      balance-type-column
;      balance-clear-column
      balance-description-column
;      balance-sign-column
      balance-amount-column
      balance-current-balance-column)
   "List of tab stops that define the start of all transaction fields.
Redefine this in your .emacs file to add or change fields.")

(defvar balance-months
  (list "Jan" "Feb" "Mar" "Apr" "May" "Jun"
        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar balance-months-regexp (regexp-opt balance-months t))

(defvar balance-mode-map nil
   "Keymap for balance buffer.")

(if balance-mode-map
   ()
   (setq balance-mode-map (make-sparse-keymap))
   (define-key balance-mode-map "\C-c\C-b" 'balance-backward-field)
   (define-key balance-mode-map "\C-c\C-c" 'balance-recalculate-buffer)
   (define-key balance-mode-map "\C-c\C-l" 'balance-recalculate-latest)
   (define-key balance-mode-map "\C-c\C-k" 'balance-recalculate-region)
   (define-key balance-mode-map "\C-c\C-x" 'balance-toggle-cleared)
   (define-key balance-mode-map "\C-c;"    'balance-toggle-comment-out)
   (define-key balance-mode-map "\C-c+"    'balance-toggle-credit)
   (define-key balance-mode-map "\C-c\C-f" 'balance-forward-field)
   (define-key balance-mode-map "\C-c\C-n" 'balance-append-next-check)
   (define-key balance-mode-map "\C-c\C-r" 'balance-summarize-checks-region)
   (define-key balance-mode-map "\C-c\C-s" 'balance-summarize-checks-buffer)
   (define-key balance-mode-map "\C-c\C-a" 'balance-append-transaction)
   (define-key balance-mode-map [tab]      'balance-forward-field)
   (define-key balance-mode-map "\C-x\C-s" 'balance-save-buffer)
   (define-key balance-mode-map "\C-k"     'balance-kill-row)
   (define-key balance-mode-map "\C-y"     'balance-yank-row)
   (define-key balance-mode-map "\C-c\C-u" 'balance-next-uncleared)
   (define-key balance-mode-map "\C-m"     'balance-toggle-hidden-cleared)   
   (define-key balance-mode-map "="        'balance-change-field))

(defvar balance-mode-abbrev-table nil
   "Abbrev table used while in balance mode.")

(define-abbrev-table 'balance-mode-abbrev-table nil)

;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun balance-mode()
"Major mode for editing a buffer containing financial transactions.
The following bindings provide the main functionality of this mode:

   \\[balance-append-transaction]\
        Append transaction to end of buffer.
   \\[balance-append-next-check]\
        Append next sequential check transaction to end of buffer.
   \\[balance-recalculate-buffer]\
        Recalculate balance of all transactions in buffer.
   \\[balance-recalculate-region]\
        Recalculate balance of all transactions in region.
   \\[balance-change-field]     \
        Change the contents of the current field.
   \\[balance-summarize-checks-buffer]\
        Produce summary of all checks in buffer.
   \\[balance-summarize-checks-region]\
        Produce summary of all checks in region.
   \\[balance-toggle-credit]\
        Change whether the current line is a debit (the default),
        a credit (indicated by \"+\"), or sets the balance (indicated
        by \"=\").

When a check is cleared by the bank, it should be noted with
   \\[balance-toggle-cleared]\
        Toggle whether or not a check is marked as cleared.
When a balance buffer is first opened, any sequences of cleared checks will
be replaced by ellipses.  Typing RET on the ellipses will display the cleared
checks, and any cleared checks can be hidden with RET.

Checks can be move about with
    \\[balance-order-latest]\
        Put the latest entries (those since the last cleared) in order by date.
    \\[balance-order-buffer]\
        Put all entries in order by date.
    \\[balance-kill-row]\
        Delete the current check.
    \\[balance-yank-row]\
        Copy the last killed check.

In addition, these bindings are defined to simplify editing
transactions:

   \\[balance-forward-field]\
        Move forward to start of next field (same as TAB).
   \\[balance-backward-field]\
        Move backward to start of previous field (same as shift-TAB).

Transactions occur on a single line and have the following fields (in
order):

   date           The transaction date in DD MMM YYYY format.
   type           This field must either be blank, a check, or match 
                  one of the expressions defined in 
                  balance-transaction-types.
   clear          Status of transaction, x is cleared.
   description    A possibly blank transaction description.
   sign           This field must either be \"+\", \"-\", \" \" or \"=\".
                  + means credit, - or space means debit, and = resets balance.
   amount         The transaction amount.  You must enter this field.
   balance        The balance after this transaction.  This field will be
                  computed upon recalculation
 (\\[balance-recalculate-buffer]).

Changing any amount and recalculating again will update all visible balances.
Transactions may be commented out by \\[balance-toggle-comment-out] (and 
uncommented out with the same command). This makes it easy to reconcile your
account with the bank, which is usually several transactions behind you.

Balance mode also provides a check summary feature.  Typing \
\\[balance-summarize-checks-buffer] will
produce a summary of all the checks in the buffer.  The transactions will be
sorted by check number and breaks in check sequence will be flagged by lines
with an asterisk on them.  In addition, the checks will be \"balanced\". Type
\\[balance-summarize-checks-buffer] to see a check \
summary for this help buffer.

To keep the file from getting too large, the command \\[balance-backup] will
make a copy of the current file (in the form `filename.n' for the smallest
number `n' for which the file doesn't already exist) and then replace any 
sequence of cleared lines by a single line with an appropriate sign and amount.

Entering balance-mode runs the `balance-mode-hooks' if any exist.  There is
also a balance-mode-abbrev-table for the truly warped.
"
   (interactive)
   (setq major-mode 'balance-mode)
   (setq mode-name "Balance")
   (use-local-map balance-mode-map)
   (make-local-variable 'tab-stop-list)
   (add-to-invisibility-spec '(balance . t))
   (setq tab-stop-list balance-tab-stop-list)
   (make-local-variable 'indent-tabs-mode)
   (setq indent-tabs-mode nil)
   (setq indent-line-function 'balance-forward-field)
   (setq local-abbrev-table balance-mode-abbrev-table)
   (auto-fill-mode -1)
   (when (= (point-min) (point-max))
       (insert balance-header "\n" balance-footer)
       (forward-line -1))
   (setq buffer-read-only t)
   (balance-fontify-buffer)
   (goto-char (point-max))
   (search-backward balance-footer)
   (forward-line -1)
   (balance-hide-cleared)
   (run-hooks 'balance-mode-hooks))

(defun balance-current-line()
"Return the current buffer line at point.  The first line is 0." 
   (save-excursion
      (beginning-of-line)
      (count-lines (point-min) (point))))

(defun balance-forward-field()
"Move the cursor to the next data entry field for the transaction on the
current line."
   (interactive)
   (move-to-tab-stop))

(defun balance-last (list)
"Return last element in a list."
   (cond
      ((null list) '())
      ((null (cdr list)) (car list))
      (t (balance-last (cdr list)))))

(defun balance-find-largest-less-than (list item)
"Search a sorted LIST of numbers, return the largest number that is still less
than ITEM."
   (let ((list-car (car list))
         (list-cdr (cdr list))
         (last nil))
      (while (and list-car (< list-car item))
         (setq last list-car)
         (setq list-car (car list-cdr))
         (setq list-cdr (cdr list-cdr)))
      last))

(defun balance-find-largest-less-than-equal(list item)
"Search a sorted LIST of numbers, return cdr of the list starting with the
largest number that is less than or equal to ITEM."
   (let ((list-car (car list))
         (list-cdr (cdr list))
         (last nil))
      (while (and list-car (<= list-car item))
         (setq last (cons list-car list-cdr))
         (setq list-car (car list-cdr))
         (setq list-cdr (cdr list-cdr)))
      last))

(defun balance-backward-field()
"Move the cursor to the previous data entry field for the transaction on the
current line."
   (interactive)
   (let* ((col (current-column))
          (prev (balance-find-largest-less-than balance-tab-stop-list col)))
      (if prev
         (move-to-column prev)
         (move-to-column (balance-last balance-tab-stop-list)))))

(defun balance-get-string (beg end)
  (let* ((true-beg (save-excursion
                     (goto-char beg)
                     (skip-chars-forward " \t" end)
                     (point)))
         (true-end (save-excursion
                     (goto-char end)
                     (skip-chars-backward " \t" true-beg)
                     (point))))
    (buffer-substring-no-properties true-beg true-end)))

(defun balance-insert-field (prompt col-beg col-width use-old history &optional change)
  (let* ((line-beg (line-beginning-position))
         (beg (+ line-beg col-beg))
         (end (+ beg col-width))
         (def-input (if use-old
                        (balance-get-string beg end)
                      ""))
         (new-input (let ((inhibit-read-only nil))
                      (if (string= prompt "Transaction type")
                          (completing-read (concat prompt ": ")
                                           balance-transaction-types
                                           nil nil def-input history)
                        (read-string (concat prompt ": ") 
                                     def-input
                                     history))))
         (inhibit-read-only t))
    (if (string= prompt "Amount")
        (setq new-input (format (concat 
                                 "%" 
                                 (number-to-string col-width) 
                                 ".2f") (string-to-number new-input)))
      (if (> (length new-input) col-width)
          (setq new-input (substring new-input 0 col-width))
        (setq new-input (format (concat 
                                 "%-"
                                 (number-to-string col-width)
                                 "s") new-input))))
    (if (and
         (eq change t)
         (string= prompt "Transaction type")
         (eq (string-match "deposit" new-input) 0))
        (balance-make-credit))
    (save-excursion
      (move-to-column (+ col-beg col-width) t))
      (delete-region beg end)
      (insert new-input)
    (unless (balance-cleared-p)
      (remove-overlays beg end)
      (overlay-put (make-overlay beg end nil t)
                   'face
                   (cond
                    ((string= prompt "Date")
                     'balance-date-face)
                    ((string= prompt "Transaction type")
                     'balance-type-face)
                    ((string= prompt "Description")
                     'balance-description-face)
                    ((string= prompt "Sign")
                     'balance-sign-face)
                    ((string= prompt "Amount")
                     'balance-amount-face))))
    (goto-char beg)
    (if (and
         (eq change 'postpone)
         (string= prompt "Transaction type")
         (eq (string-match "deposit" new-input) 0))
        'credit)))

(defun balance-insert-date ()
  (balance-insert-field "Date" 
                        balance-date-column 
                        balance-date-width 
                        t
                        'balance-date-history))

(defvar balance-type-history nil)
(defun balance-insert-transaction-type (&optional change)
  (balance-insert-field "Transaction type" 
                        balance-type-column 
                        balance-type-width 
                        t
                        'balance-type-history
                        change))

(defvar balance-description-history nil)
(defun balance-insert-description ()
  (balance-insert-field "Description" 
                        balance-description-column 
                        balance-description-width 
                        t
                        'balance-description-history))

;(defun balance-insert-sign ()
;  (balance-insert-field "Sign" balance-sign-column balance-sign-width t))

(defvar balance-amount-history nil)
(defun balance-insert-amount ()
  (balance-insert-field "Amount" 
                        balance-amount-column 
                        balance-amount-width 
                        t
                        'balance-amount-history))

(defun balance-change-field ()
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at (concat "^[0-9]+ " balance-months-regexp " [0-9]+")))
      (let ((col (current-column)))
        (cond ((< col balance-type-column)
               (balance-insert-date))
              ((< col balance-description-column)
               (balance-insert-transaction-type t))
              ((< col balance-sign-column)
               (balance-insert-description))
              ((< col balance-amount-column)
               nil)
;               (balance-insert-sign))
              ((< col balance-current-balance-column)
               (balance-insert-amount))))))

(defun balance-cleared-p ()
  (save-excursion
    (move-to-column balance-clear-column)
    (looking-at "x")))

(defun balance-next-uncleared ()
  (interactive)
  (forward-line 1)
  (while (balance-cleared-p)
    (forward-line 1)))

(defun balance-toggle-cleared ()
  (interactive)
  (let (xed)
    (when (save-excursion
            (beginning-of-line)
            (looking-at (concat "^[0-9]+ " balance-months-regexp " [0-9]+")))
      (save-excursion
        (move-to-column balance-clear-column)
        (let ((pt (point))
              (inhibit-read-only t))
          (if (setq xed (string= (buffer-substring pt (1+ pt)) "x"))
              (progn
                (delete-char 1)
                (insert " "))
            (delete-char 1)
            (insert "x"))
          (balance-fontify-line)))
      (unless xed
          (balance-next-uncleared)))))

(defun balance-cleared-but-not-backed-up-p ()
  (and
   (balance-cleared-p)
   (not (string= (balance-get-type) "cleared    "))))

(defun balance-cleared-or-backed-up-p ()
  (or
   (balance-cleared-p)
   (string= (balance-get-type) "cleared    ")))

(defun balance-hide-cleared-lines ()
  (interactive)
  (save-excursion
    (when (balance-cleared-p) ;(balance-cleared-but-not-backed-up-p)
      (forward-line -1)
      (while (and
              (balance-cleared-p) ;(balance-cleared-but-not-backed-up-p)
              (> (point) (point-min)))
        (forward-line -1))
      (if (not (balance-cleared-p)) ;(balance-cleared-but-not-backed-up-p))
          (forward-line 1))
      (let ((beg (point))
            end)
        (while (balance-cleared-p) ;(balance-cleared-but-not-backed-up-p)
          (forward-line 1))
        (overlay-put (make-overlay beg (1- (point)) nil t)
                     'invisible 'balance)))))

(defun balance-show-hidden-lines ()
  (let ((beg
         (save-excursion
           (forward-line 0)
           (while (and
                   (get-char-property (point) 'invisible)
                   (> (point) (point-min)))
             (forward-line -1))
           (point)))
        (end
         (save-excursion
           (forward-line 1)
           (while (get-char-property (point) 'invisible)
             (forward-line 1))
           (point))))
    (remove-overlays beg end 'invisible 'balance)))

(defun balance-toggle-hidden-cleared ()
  (interactive)
  (if (not (get-char-property (line-beginning-position) 'invisible))
      (balance-hide-cleared-lines)
    (balance-show-hidden-lines)))
    
(defun balance-hide-cleared ()
  (interactive)
  (let (beg)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (looking-at balance-footer))
        (while (and
                (not (balance-cleared-p))
                (not (looking-at balance-footer)))
          (forward-line 1))
        (setq beg (point))
        (while (balance-cleared-p)
          (forward-line 1))
        (unless (= beg (point))
          (overlay-put (make-overlay beg (1- (point)) nil t)
                       'invisible 'balance))
        (if (not (looking-at balance-footer))
            (forward-line 1))))))
  
(defun balance-insert-line (date clear type description sign amount)
  (let ((inhibit-read-only t))
    (forward-line 0)
    (insert "\n")
    (forward-char -1)
    (move-to-column balance-date-column t)
    (insert date)
    (move-to-column balance-clear-column t)
    (insert clear)
    (move-to-column balance-type-column t)
    (insert type)
    (move-to-column balance-description-column t)
    (insert description)
    (move-to-column balance-sign-column t)
    (insert sign)
    (move-to-column balance-amount-column t)
    (insert (format (concat 
                     "%" 
                   (number-to-string balance-amount-width) 
                   ".2f") amount))
    (balance-fontify-line)))

(defun balance-get-date ()
  (save-excursion
    (move-to-column balance-date-column)
    (let ((pt (point)))
      (buffer-substring-no-properties pt
                                      (+ pt balance-date-width)))))

(defun balance-get-sign ()
  (save-excursion
    (move-to-column balance-sign-column)
    (let ((pt (point)))
      (buffer-substring-no-properties pt
                                      (+ pt balance-sign-width)))))

(defun balance-get-type ()
  (save-excursion
    (move-to-column balance-type-column)
    (let ((pt (point)))
      (buffer-substring-no-properties pt
                                      (+ pt balance-type-width)))))

(defun balance-remove-cleared ()
  (let (beg date sign)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (looking-at balance-footer))
        (while (and
                (not (balance-cleared-p))
                (not (looking-at balance-footer)))
          (forward-line 1))
        (setq beg (point))
        (setq sign "")
        (while (balance-cleared-p)
          (setq date (balance-get-date))
          (if (string= (balance-get-sign) "=")
              (setq sign "="))
          (forward-line 1))
        (unless (= beg (point))
          (let ((inhibit-read-only t)
                (amt (car (balance-recalculate beg (point)))))
            (delete-region beg (point))
            (if (string= sign "=")
                (if (< amt 0)
                    (progn
                      (balance-insert-line date "x" "cleared" "" "" (- amt))
                      (balance-insert-line date "x" "cleared" "" sign 0))
                  (balance-insert-line date "x" "cleared" "" sign amt))
              (if (< amt 0)
                  (setq amt (- amt))
                (setq sign "+"))
              (balance-insert-line date "x" "cleared" "" sign amt))))
        (if (not (looking-at balance-footer))
            (forward-line 1))))))

(defun balance-backup ()
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name))
        (n 1))
    (while (file-exists-p 
            (concat file "." (number-to-string n)))
      (setq n (1+ n)))
    (save-excursion
      (find-file (concat file "." (number-to-string n)))
      (let ((buffer-read-only nil))
        (insert-file-contents file)
        (save-buffer))
      (kill-buffer (current-buffer))))
  (balance-remove-cleared)
  (save-buffer))

(defun balance-toggle-credit ()
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at (concat "^[0-9]+ " balance-months-regexp " [0-9]+")))
    (save-excursion
      (move-to-column balance-sign-column)
      (let* ((pt (point))
             (inhibit-read-only t)
             (sgn (buffer-substring pt (1+ pt))))
        (delete-char 1)
        (if (string= sgn " ")
            (insert "+")
          (if (string= sgn "+")
              (insert "=")
            (insert " ")))
        (balance-fontify-line)))))

(defun balance-make-credit ()
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at (concat "^[0-9]+ " balance-months-regexp " [0-9]+")))
    (save-excursion
      (move-to-column balance-sign-column)
      (let* ((inhibit-read-only t))
        (delete-char 1)
        (insert "+")))
    (balance-fontify-line)))

(defvar balance-date-history nil)
(defun balance-append-transaction(&optional arg)
"Add a transaction to the end of current buffer using today's date."
   (interactive "P")
   (let* ((inhibit-read-only t)
          (date (calendar-current-date))
          (month (car date))
          (cr nil)
          (day (car (cdr date)))
          (year (car (cdr (cdr date)))))
     (if arg
         (progn
           (beginning-of-line)
           (unless (or
                    (looking-at (concat "^[0-9]+ " balance-months-regexp " [0-9]+"))
                    (and (looking-at "^==")
                         (save-excursion
                           (forward-line -1)
                           (looking-at "^=="))))
             (error "Can't insert transaction on this line.")))
       (goto-char (point-max))
       (re-search-backward "^=="))
     (insert "\n")
     (forward-line -1)
     (insert (format (concat "%-" 
                             (number-to-string balance-date-width)
                             "s")
                     (let ((inhibit-read-only nil))
                       (read-string "Date (DD MMM YYYY): "
                                    (format "%02d %s %04d" 
                                            day (nth (1- month) balance-months) year)
                                    'balance-date-history))))
     (overlay-put (make-overlay (line-beginning-position)
                                (+ (line-beginning-position) balance-date-width)
                                nil t)
                  'face 'balance-date-face)
     (move-to-column balance-type-column t)
     (setq cr
           (balance-insert-field "Transaction type" 
                                 balance-type-column 
                                 balance-type-width 
                                 nil
                                 'balance-type-history
                                 'postpone))
     (move-to-column balance-description-column t)
     (balance-insert-field "Description" 
                           balance-description-column 
                           balance-description-width 
                           nil
                           'balance-description-history)
;     (move-to-column balance-sign-column t)
;     (balance-insert-field "Sign" 
;                           balance-sign-column 
;                           balance-sign-width nil)
     (move-to-column balance-amount-column t)
     (balance-insert-field "Amount" 
                           balance-amount-column 
                           balance-amount-width 
                           nil
                           'balance-amount-history)
     (beginning-of-line)
     (if (eq cr 'credit)
         (balance-make-credit))))

(defun balance-append-next-check (&optional arg)
"Add a check transaction to the end of the current buffer using today's date.
Inserts the check number following the last check number written into the
transaction type column.  Loses if you write checks out of order."
   (interactive "P")
   (let* ((inhibit-read-only t)
          (date (calendar-current-date))
          (month (car date))
          (day (car (cdr date)))
          (year (car (cdr (cdr date)))))
     (if arg
         (progn
           (beginning-of-line)
           (unless (or
                    (looking-at (concat "^[0-9]+ " balance-months-regexp " [0-9]+"))
                    (and (looking-at "^==")
                         (save-excursion
                           (forward-line -1)
                           (looking-at "^=="))))
             (error "Can't insert transaction on this line.")))
       (goto-char (point-max))
       (re-search-backward "^=="))
     (insert "\n")
     (forward-line -1)
     (insert (format (concat "%-" 
                             (number-to-string balance-date-width)
                             "s")
                     (let ((inhibit-read-only nil))
                       (read-string "Date (DD MMM YYYY): "
                                    (format "%02d %s %04d" 
                                            day (nth (1- month) balance-months) year)
                                    'balance-date-history))))
     (overlay-put (make-overlay (line-beginning-position)
                                (point) nil t)
                  'face 'balance-date-face)
     (move-to-column balance-type-column t)
     (let ((beg (point))
           check check-number)
       (save-excursion
         (if (search-backward-regexp balance-check-type 0 t)
             (setq check (buffer-substring (match-beginning 1) (match-end 1)))
           (error "no previous checks found")))
       (setq check-number (1+ (string-to-number check)))
       (insert (format "check %d" check-number))
       (move-to-column balance-description-column t)
       (overlay-put (make-overlay beg (point) nil t)
                    'face 'balance-type-face))
     (balance-insert-field "Description" 
                           balance-description-column 
                           balance-description-width 
                           nil
                           'balance-description-history)
;     (move-to-column balance-sign-column t)
;     (balance-insert-field "Sign" 
;                           balance-sign-column 
;                           balance-sign-width nil)
     (move-to-column balance-amount-column t)
     (balance-insert-field "Amount" 
                           balance-amount-column 
                           balance-amount-width 
                           nil
                           'balance-amount-history)
     (beginning-of-line)))

(defun balance-find-next-transaction (&optional arg)
"Find next line that looks like a complete transaction and return a list of
the line start, numeric data start and line end points."
   (let ((inhibit-read-only t)
         (found nil)
         (line-regexp (concat "^"
                              (if arg "%?" "")
                              "[0-9]+ "
                              balance-months-regexp
                              " [0-9]+.*$"))
         line-start
         line-end
         data-start)
      (while (and
               (not found)
               (search-forward-regexp line-regexp (point-max) t))
         (setq line-start (match-beginning 0))
         (untabify (match-beginning 0) (match-end 0))
         (setq line-end (progn (end-of-line) (point)))
         (setq data-start (+ line-start balance-sign-column))
         (if (> line-end data-start)
               (setq found t)))
      (if found
         (list line-start data-start line-end)
         nil)))

(defun balance-parse-transaction-data(data)
"Given a STRING representing the sign, amount and optionally balance of a
transaction, return a list of the sign and amount and balance as floating
point numbers.  Balance is nil if not present."
   (let ((data-regexp "\\([-+= ]\\)[ \t]*\\([0-9.]+\\)?[ \t]*\\([-]?[0-9.]+\\)?")
         (balance nil)
         (reset nil)
         string sign amount)
      (string-match data-regexp data)
      (if (match-beginning 1)
         (setq sign (substring data (match-beginning 1) (match-end 1)))
         (error "line %d, missing sign" (1+ (balance-current-line))))
      (if (equal sign " ")
          (progn 
            (setq sign "-")))
      (if (equal "=" sign)
         (progn
            (setq sign "+")
            (setq reset t)))
      (if (match-beginning 2)
         (progn
            (setq string (substring data (match-beginning 2) (match-end 2)))
            (setq amount (string-to-number (concat sign string))))
         (error "line %d, missing amount" (1+ (balance-current-line))))
      (if (match-beginning 3)
         (progn
            (setq string (substring data (match-beginning 3) (match-end 3)))
            (setq balance (string-to-number string))))
      (if reset (setq sign "="))
      (list sign amount balance)))

(defun balance-same(amount1 amount2)
"Compare two dollar amounts for equivalence.  This is necessary due to the
imprecision of the float implementation in emacs 19."
(let ((string1 (format "%10.2f" amount1))
      (string2 (format "%10.2f" amount2)))
  (equal string1 string2)))

(defun balance-form-transaction-data(sign amount balance)
"Given SIGN, AMOUNT and a BALANCE, return a string suitable for placing in the
numeric region of a transaction, based on the defined input columns."
   (let* ((amount (abs amount))
         (width1 (- balance-amount-column balance-sign-column))
         (value (concat "%" (number-to-string balance-amount-width) ".2f"))
         (format-string (concat "%-" (number-to-string width1) 
                                "s" value 
                                "%1s" value)))
     (if (string= sign "-")
         (setq sign " "))
     (format format-string sign amount "" balance)))

(defface balance-comment-out-face
  '((t (:slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-header-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-footer-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-date-face
  '((t (:slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-clear-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-type-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-description-face
  '((t (:bold t
        :foreground "blue")))
  "Face used to highlight search matches in search result buffer.")

(defface balance-sign-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-amount-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-current-balance-face
  '((t (:bold t
        :slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface balance-cleared-face
  '((t (:foreground "dim gray"
        :slant italic)))
;  '((t (:weight light
;        :slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defun balance-fontify-line ()
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (remove-overlays beg end)
    (if (balance-cleared-p)
        (overlay-put (make-overlay beg end nil t)
                     'face 'balance-cleared-face)
      (if (string= (buffer-substring beg (1+ beg)) "%")
          (overlay-put (make-overlay beg end nil t)
                     'face 'balance-comment-out-face)
        (overlay-put (make-overlay (+ beg balance-date-column)
                                   (+ beg balance-date-column balance-date-width)
                                   nil t)
                     'face 'balance-date-face)
        (overlay-put (make-overlay (+ beg balance-clear-column)
                                   (+ beg balance-clear-column balance-clear-width)
                                   nil t)
                     'face 'balance-clear-face)
        (overlay-put (make-overlay (+ beg balance-type-column)
                                   (+ beg balance-type-column balance-type-width)
                                   nil t)
                     'face 'balance-type-face)
        (overlay-put (make-overlay (+ beg balance-description-column)
                                   (+ beg balance-description-column 
                                      balance-description-width)
                                   nil t)
                     'face 'balance-description-face)
        (overlay-put (make-overlay (+ beg balance-sign-column)
                                   (+ beg balance-sign-column balance-sign-width)
                                   nil t)
                     'face 'balance-sign-face)
        (overlay-put (make-overlay (+ beg balance-amount-column)
                                   (+ beg balance-amount-column balance-amount-width)
                                   nil t)
                     'face 'balance-amount-face)
        (overlay-put (make-overlay (+ beg balance-current-balance-column)
                                   (line-end-position)
                                   nil t)
                     'face 'balance-current-balance-face)))))
  
(defun balance-fontify-buffer ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (overlay-put (make-overlay (point-min) (point) nil t) 'face 'balance-header-face)
    (while (balance-find-next-transaction t)
      (balance-fontify-line))
    (forward-line 1)
    (overlay-put (make-overlay (point) (point-max) nil t) 'face 'balance-footer-face)))

(defun balance-fontify-region (beg end)
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (forward-line 2)
    (overlay-put (make-overlay (point-min) (point) nil t) 'face 'balance-header-face)
    (while (and
            (balance-find-next-transaction t)
            (< (point) end))
      (balance-fontify-line))))

(defun balance-before (line1 line2)
  (let (num1 num2
        (date1 (substring line1 0 2))
        (date2 (substring line2 0 2))
        (month1 (substring line1 3 6))
        (month2 (substring line2 3 6))
        (year1 (substring line1 7 11))
        (year2 (substring line2 7 11)))
    (setq month1 (- 13 (length (member month1 balance-months))))
    (setq month2 (- 13 (length (member month2 balance-months))))
    (setq num1 (concat year1 (format "%02d" month1) date1))
    (setq num2 (concat year2 (format "%02d" month2) date2))
    (<= (string-to-number num1) (string-to-number num2))))

(defun balance-compare-rows ()
  (let ((line1 (buffer-substring-no-properties 
                (line-beginning-position) (line-end-position)))
        (line2 (save-excursion
                 (forward-line 1)
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))
    (balance-before line1 line2)))

(defun balance-switch-rows ()
  (let ((inhibit-read-only t))
    (save-excursion
      (let ((pt (line-beginning-position)))
        (forward-line 1)
        (let ((oldline (buffer-substring pt (point))))
          (delete-region pt (point))
          (forward-line 1)
          (insert oldline))))))

(defun balance-order-region (beg end)
  "Put the records in order by date."
  (let (pt n)
    (save-excursion
      (goto-char beg)
      (setq pt (line-beginning-position))
      (goto-char end)
      (setq n (+ 1 (count-lines pt (line-beginning-position))))
      (let ((switch t)
            (ord)
            (i))
        (while (and switch (> n 1))
          (setq switch nil)
          (goto-char pt)
          (setq i 1)
          (while (< i n)
            (unless (balance-compare-rows)
              (balance-switch-rows)
              (setq switch t))
            (forward-line 1)
            (setq i (1+ i)))
          (setq n (- n 1)))))))

(defun balance-order-buffer ()
  (interactive)
  (remove-overlays (point-min) (point-max))
  (save-excursion
    (let (beg end)
      (goto-char (point-min))
      (re-search-forward (concat "^%?[0-9]+ " balance-months-regexp " [0-9]+"))
      (setq beg (line-beginning-position))
      (goto-char (point-max))
      (re-search-backward (concat "^%?[0-9]+ " balance-months-regexp " [0-9]+"))
      (setq end (line-beginning-position))
      (remove-overlays beg end)
      (balance-order-region beg end))
    (balance-fontify-buffer)))

(defun balance-order-latest ()
  (interactive)
  (save-excursion
    (let (beg end fend)
      (goto-char (point-max))
      (re-search-backward (concat "^%?[0-9]+ " balance-months-regexp " [0-9]+"))
      (beginning-of-line)
      (setq end (point))
      (setq fend (1+ (line-end-position)))
      (forward-line -1)
      (while
          (and
;           (not (get-char-property (point) 'invisible))
           (not (balance-cleared-p))
           (looking-at (concat "^%?[0-9]+ " balance-months-regexp " [0-9]+")))
        (forward-line -1))
      (forward-line 1)
      (let ((pt (point)))
        (balance-order-region pt end)
        (balance-fontify-region pt fend)))))

(defun balance-toggle-comment-out ()
  (interactive)
  (let* ((inhibit-read-only t)
         (beg (line-beginning-position))
         (end (+ beg balance-date-width 1)))
    (save-excursion
      (beginning-of-line)
      (remove-overlays beg (line-end-position))
      (if (looking-at "^%")
          (progn
            (delete-char 1)
            (forward-char balance-date-width)
            (insert " ")
            (balance-fontify-line))
        (insert "%")
        (forward-char balance-date-width)
        (delete-char 1)
        (overlay-put (make-overlay beg (line-end-position) nil t)
                     'face 'balance-comment-out-face)))))

(defvar balance-killed-rows nil)
(make-variable-buffer-local 'balance-killed-rows)

(defun balance-kill-row ()
  (interactive)
  (let ((inhibit-read-only t)
        (pt (line-beginning-position)))
    (push (buffer-substring-no-properties pt (line-end-position))
          balance-killed-rows)
    (remove-overlays pt (line-end-position))
    (forward-line 1)
    (delete-region pt (point))))

(defun balance-yank-row ()
  (interactive)
  (if (<= (balance-current-line) 1)
      (message "Can't yank row here.")
    (goto-char (line-beginning-position))
    (if balance-killed-rows
        (let ((inhibit-read-only t))
          (insert (pop balance-killed-rows) "\n")
          (forward-line -1)
          (balance-fontify-line))
      (message "No killed rows."))))

(defun balance-recalculate (start end)
"Go through the region specified by START and END and recalculate all
transaction balances.  The final balance, uncleared total, and 
the number of balances that changed, and the transaction count are 
returned in a list."
   (let ((inhibit-read-only t)
         (current-balance 0)
         (changes 0)
         (uncleared 0)
         (transactions 0)
         line-points)
      (save-excursion (save-restriction
         (narrow-to-region start end)
         (goto-char (point-min))
         (while (setq line-points (balance-find-next-transaction))
            (setq transactions (1+ transactions))
            (let* ((line-start (nth 0 line-points))
                  (data-start (nth 1 line-points))
                  (data-end (nth 2 line-points))
                  (clear-flag (buffer-substring 
                               (+ line-start balance-clear-column) 
                               (+ 1 line-start balance-clear-column)))
                  (data-string (buffer-substring data-start data-end))
                  (data-values (balance-parse-transaction-data data-string))
                  (sign (nth 0 data-values))
                  (amount (nth 1 data-values))
                  (balance (nth 2 data-values))
                  (new-balance (if (equal sign "=")
                                  amount
                                  (+ current-balance amount)))
                  (new-uncleared (if (equal clear-flag "x")
                                 uncleared
                                  (+ uncleared amount)))
                  (new-string
                     (balance-form-transaction-data sign amount new-balance)))
               (setq current-balance new-balance)
               (setq uncleared new-uncleared)
               (if (or (null balance) (not (balance-same balance new-balance)))
                  (setq changes (1+ changes)))
               (if (not (equal data-string new-string))
                  (progn
                     (delete-region data-start data-end)
                     (goto-char data-start)
                     (insert new-string)
                     (balance-fontify-line)))))
         (widen)))
;      (balance-fontify-buffer)
;      (balance-hide-cleared)
      (list current-balance uncleared changes transactions)))

(defun balance-recalculate-buffer()
"Recalculate the current buffer.  See balance-recalculate."
   (interactive)
   (let* ((result (balance-recalculate (point-min) (point-max)))
         (balance (nth 0 result))
         (uncleared (nth 1 result))
         (changes (nth 2 result))
         (total (nth 3 result)))
      (message 
       (format "book bal %.2f unclrd %.2f bank bal %.2f (%d/%d recalcs)"
               balance uncleared (- balance uncleared) changes total))
      (if (> changes 0)
         (end-of-line))))
;   (balance-fontify-buffer))

(defun balance-recalculate-region(start end)
"Recalculate the current region.  See balance-recalculate."
   (interactive "r")
   (let* ((result (balance-recalculate start end))
         (balance (nth 0 result))
         (uncleared (nth 1 result))
         (changes (nth 2 result))
         (total (nth 3 result)))
     (message (format "region balance %.2f uncleared %.2f (%d/%d recalcs)"
                      balance uncleared changes total))
      (if (> changes 0)
         (end-of-line))))

(defun balance-recalculate-latest ()
"Go through the region specified by START and END and recalculate all
transaction balances.  The final balance, uncleared total, and 
the number of balances that changed, and the transaction count are 
returned in a list."
   (interactive)
   (let (string
         (inhibit-read-only t)
         (current-balance)
         (changes 0)
         (uncleared 0)
         (transactions 0)
         line-points
         end)
     (save-excursion
       (goto-char (point-max))
       (re-search-backward 
        (concat "^%?[0-9]+ " balance-months-regexp " [0-9]+"))
       (beginning-of-line)
       (setq end (point))
       (forward-line -1)
       (while
           (and
            (not (balance-cleared-p))
            (looking-at (concat "^%?[0-9]+ " balance-months-regexp " [0-9]+")))
         (forward-line -1))
       (if (not (balance-cleared-p))
           (balance-recalculate-buffer)
         (let ((data-regexp "\\([0-9.]+\\)$")
               (data (buffer-substring-no-properties 
                      (line-beginning-position)
                      (line-end-position))))
           (string-match data-regexp data)
           (setq string (match-string 1 data))
           (setq current-balance (string-to-number string))
         (forward-line 1)
;       (save-restriction
;         (narrow-to-region (point) end)
;         (goto-char (point-min))
           (while (setq line-points (balance-find-next-transaction))
             (setq transactions (1+ transactions))
             (let* ((line-start (nth 0 line-points))
                    (data-start (nth 1 line-points))
                    (data-end (nth 2 line-points))
                    (clear-flag (buffer-substring 
                                 (+ line-start balance-clear-column) 
                                 (+ 1 line-start balance-clear-column)))
                    (data-string (buffer-substring data-start data-end))
                    (data-values (balance-parse-transaction-data data-string))
                    (sign (nth 0 data-values))
                    (amount (nth 1 data-values))
                    (balance (nth 2 data-values))
                    (new-balance (if (equal sign "=")
                                     amount
                                   (+ current-balance amount)))
                    (new-uncleared (if (equal clear-flag "x")
                                       uncleared
                                     (+ uncleared amount)))
                    (new-string
                     (balance-form-transaction-data sign amount new-balance)))
               (setq current-balance new-balance)
               (setq uncleared new-uncleared)
               (if (or (null balance) (not (balance-same balance new-balance)))
                   (setq changes (1+ changes)))
               (if (not (equal data-string new-string))
                   (progn
                     (delete-region data-start data-end)
                     (goto-char data-start)
                     (insert new-string)
                     (balance-fontify-line)))))
           (list current-balance uncleared changes transactions)))
       (message 
        (format "book bal %.2f unclrd %.2f bank bal %.2f (%d/%d recalcs)"
                current-balance uncleared 
                (- balance uncleared) changes transactions))
       (if (> changes 0)
           (end-of-line)))))

(defun balance-save-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-buffer)))

(defun balance-summarize-checks(start end)
"Create a *checks* buffer that lists only the checks in the specified region.
The list is sorted on check number.  Breaks in sequence are denoted by lines
containing an asterisk between the checks where the break occurs.  The buffer
is also recalculated, thus showing to total of the checks summarized."
   (let ((balance-buffer (current-buffer))
         (summary-buffer (get-buffer-create "*check summary*"))
         (check-count 0)
         (sequence-breaks 0)
         line-points)
      (save-excursion (save-restriction
         (set-buffer summary-buffer)
         (delete-region (point-min) (point-max))
         (set-buffer balance-buffer)
         (narrow-to-region start end)
         (goto-char (point-min))
         (while (setq line-points (balance-find-next-transaction))
            (let* ((line-start (nth 0 line-points))
                  (line-end (nth 2 line-points))
                  (type-start (+ line-start balance-type-column))
                  (type-end (+ line-start balance-description-column))
                  (type-string (buffer-substring type-start type-end)))
               (if (string-match balance-check-type type-string)
                  (progn
                     (append-to-buffer
                        summary-buffer line-start (1+ line-end))
                     (setq check-count (1+ check-count))))))
         (widen)
         (set-buffer summary-buffer)
         (sort-numeric-fields 3 (point-min) (point-max))
         (setq sequence-breaks (balance-find-sequence-breaks))
         (goto-char (point-max))
         (insert (format "\n%d check%s summarized, %d sequence break%s\n"
                     check-count
                     (if (equal 1 check-count) "" "s")
                     sequence-breaks
                     (if (equal 1 sequence-breaks) "" "s")))
         (set-buffer balance-buffer)))
      (pop-to-buffer summary-buffer)
      (goto-char (point-max))))

(defun balance-find-sequence-breaks()
"Find check sequence breaks in the current check summary buffer.  Mark breaks
in sequence by inserting a line with an asterisk between the offending checks.
Return the count of sequence breaks found."
   (let ((last-check nil)
         (sequence-breaks 0))
      (goto-char (point-min))
      (while (search-forward-regexp
               (concat "\\([0-9/]+\\)[ \t]+" balance-check-type)
               (point-max) t)
         (let* ((check-start (match-beginning 2))
               (check-end (match-end 2))
               (check-string (buffer-substring check-start check-end))
               (check-number (string-to-number check-string)))
         (if (not last-check)
            (setq last-check check-number)
            (if (not (equal check-number (1+ last-check)))
               (progn
                  (setq sequence-breaks (1+ sequence-breaks))
                  (beginning-of-line)
                  (open-line 1)
                  (insert "*")
                  (next-line 2)))
            (setq last-check check-number))))
      sequence-breaks))

(defun balance-summarize-checks-buffer()
   (interactive)
   (balance-summarize-checks (point-min) (point-max))
   (balance-recalculate (point-min) (point-max)))

(defun balance-summarize-checks-region(start end)
   (interactive "r")
   (balance-summarize-checks start end)
   (balance-recalculate (point-min) (point-max)))

(provide 'balance)
