;; -*-scheme-*-

(define-module (gnucash report user-reports dev-budget))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))
(use-modules (gnucash report eguile-gnc))


(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;;for gnc-build-url
(gnc:module-load "gnucash/html" 0)

(use-modules (ice-9 regex))  ; for regular expressions
(use-modules (srfi srfi-13)) ; for extra string functions

;;(gnc:module-load "gnucash/report/user-reports/budget-utils" 0)
(use-modules (gnucash report user-reports budget-utils))


(define (find-template fname)
  ;; Find the eguile template file 'fname', and return its full path.
  ;; First look in the user's .gnucash directory.
  ;; Then look in Gnucash's standard report directory.
  ;; This is complicated because of the need to cater for
  ;; various operating systems; so it takes a fairly heuristic,
  ;; 'best guess' approach.
  ;; If no file is found, returns just 'fname' for use in error messages.
  ;; Note: this has been tested on Linux and Windows Vista so far...
  ;; Another note: this routine would be better in eguile-gnc.scm, but
  ;; slib breaks eguile-gnc by turning off case-sensitivity when
  ;; evaluating the template.
  (let* ((userdir (sub-vicinity (user-vicinity) ".gnucash"))
         (sysdir  (sub-vicinity (sub-vicinity (user-vicinity) "gnucash") "report"))
         (home (or (home-vicinity)
                   (getenv "USERPROFILE")
                   (user-vicinity)
                   "")))
    ; make sure there's a trailing delimiter
    (set! home (sub-vicinity (user-vicinity) home))
    (let ((home-template (in-vicinity (in-vicinity home userdir) fname)))
      (if (access? home-template R_OK)
        home-template
        (or (%search-load-path (in-vicinity sysdir fname))
            fname)))))

(define (escape-html s1)
  ;; convert string s1 to escape HTML special characters < > and &
  ;; i.e. convert them to &lt; &gt; and &amp; respectively.
  ;; Maybe there's a way to do this in one go... (but order is important)
  (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
  (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
  (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))

(define (dump x) (escape-html (object->string x)))

(define (account-link account)
  ;; Return an HTML link to the given account,
  ;; e.g. <a href="gnc-register:acct-guid=abcdeaalsdfjkalsdk#">Account Name</a>
;;  (if opt-use-links?
      (string-append
       "<a href=\"gnc-register:acct-guid="
       (gncAccountGetGUID account)
       "\">"
       (xaccAccountGetName account)
       "</a>") )
;;      (xaccAccountGetName account)))

(define (display-value-list cc-list comm)
  (cond
   ( (null? cc-list) '() )
   (else (let ( (cc (car cc-list)) )
           (cons (gnc:gnc-monetary-amount (cc 'getmonetary comm #f)) (display-value-list (cdr cc-list) comm)))
         )))




(define (budget-record-list-printer record-list level port)
  (cond
   ((null? record-list) '())
   (else (let ( (rec (car record-list)) )
           (define print-indent
             (lambda (level)
               (cond ((= level 0) '())
                     (else (display "+")
                           (print-indent (- level 1))))))
           (display "<br>")
           (print-indent level)
           (budget-record-printer rec port)
           (budget-record-list-printer (budget-record-children rec) (+ level 1) port)
           (budget-record-list-printer (cdr record-list) level port) ))))

;; Budget record
(define (budget-record-printer budget-record port)
    ;; budget-record printer.  This is for debugging reports, so it uses
    ;; HTML for pretty-printing
    (set-current-output-port port)
    (display " budget-record:: ")
    (display " account: ")     (display (dump (xaccAccountGetName (budget-record-account budget-record))))
;;    (display " actual-cc-list: ")     (display (display-value-list (budget-record-actual-cc budget-record) (budget-record-commodity budget-record)))
;;    (display " budget-cc-list: ")     (display (display-value-list (budget-record-budget-cc budget-record) (budget-record-commodity budget-record)))
;;    (display " children: ")     (budget-record-list-printer (budget-record-children budget-record) port)

;;    (display " code: ")        (display (budget-record-code budget-record))
;;    (display " placeholder: ") (display (dump (budget-record-placeholder? budget-record)))
;;    (display " namelink: ")    (display (budget-record-namelink budget-record))
;;    (display " commodity: ")   (if (budget-record-commodity budget-record)
;;                                   (display (gnc-commodity-get-mnemonic (budget-record-commodity budget-record)))
;;                                   (display "#f"))
;;    (display " balance-num: ") (if (budget-record-balance-num budget-record)
;;                                   (display (gnc-numeric-to-double (budget-record-balance-num budget-record)))
;;                                        ;(display (gnc:monetary->string (budget-record-balance-mny budget-record)))
;;                                        ;(display (format-monetary (budget-record-balance-num budget-record))) ; not this -- too fancy
;;                                   (display "#f"))
;;    (display " depth: ")       (display (budget-record-depth budget-record))
;;    (display " treedepth: ")   (display (budget-record-treedepth budget-record))
;;    (display " non-zero?: ")   (display (budget-record-non-zero? budget-record))
;;    (display " summary?: ")    (display (budget-record-summary? budget-record))
;;    (display " subtotal-cc: ") (if (budget-record-subtotal-cc budget-record)
;;                                        ;(display (get-comm-coll-total (budget-record-subtotal-cc budget-record) #f))
;;                                        ;(display (format-comm-coll (budget-record-subtotal-cc budget-record)))
;;                                   (display
;;                                    (string-concatenate
;;                                     (map-in-order
;;                                      (lambda (mny)
;;                                        (string-append (gnc:monetary->string mny) " "))
;;                                      ((budget-record-subtotal-cc budget-record) 'format gnc:make-gnc-monetary #f))))
;;                                   (display "#f"))
;;    (display " sublist: ")     (if (budget-record-sublist budget-record)
;;                                   (begin
;;                                     (display "\n<ul>")
;;                                     (for sub-budget-record in (budget-record-sublist budget-record) do
;;                                          (display "\n<li>")
;;                                          (budget-record-printer sub-budget-record port)
;;                                          (display "</li>"))
;;                                     (display "</ul>"))
;;                                   (display "#f"))
    )

(define <budget-record> (make-record-type "<budget-record>"
                                          '(account
                                            code
                                            placeholder?
                                            namelink ; a/c name, as link if required
                                            commodity
                                            actual-cc
                                            budget-cc
                                            diff-cc
                                            actual-total-cc
                                            budget-total-cc
                                            diff-total-cc
                                            children

                                            balance-num ; excluding sublist
                                            depth
                                            treedepth
                                            non-zero?  ; #t if this or any sub-a/cs are non zero
                                            summary?   ; #t if subaccounts summarised here
                                            subtotal-cc ; of sublist plus this a/c
                                            sublist)
                                          budget-record-printer))
(define (blank-budget-record)
  ((record-constructor <budget-record>)
   #f         ; account
   ""         ; code
   #f         ; placeholder?
   ""         ; namelink
   (gnc-default-currency)         ; commodity
   (gnc:make-commodity-collector)
   (gnc:make-commodity-collector)
   (gnc:make-commodity-collector)
   (gnc:make-commodity-collector)
   (gnc:make-commodity-collector)
   (gnc:make-commodity-collector)
   '()
   (gnc-numeric-zero) ; balance-num
   0         ; depth
   0         ; treedepth
   #f         ; non-zero?
   #f        ; summary?
   (gnc:make-commodity-collector) ; subtotal-cc
   #f        ;'()        ; sublist
   ))

;;  (define newbudgetrec-full (record-constructor budgetrectype))                ; requires all the fields
;;  (define newbudgetrec-empty (record-constructor budgetrectype '()))        ; all fields default to #f
;;  (define newbudgetrec (record-constructor budgetrectype '(account         ; most-likely-to-be-needed fields
;;                                                           code
;;                                                           placeholder?
;;                                                           namelink
;;                                                           commodity
;;                                                           balance-num
;;                                                           depth
;;                                                           treedepth)))
;;
;;  (define (newbudgetrec-clean)
;;    ;; Create a new accrec with 'clean' empty values, e.g. strings are "", not #f
;;    (newbudgetrec-full #f         ; account
;;                       ""         ; code
;;                       #f         ; placeholder?
;;                       ""         ; namelink
;;                       (gnc-default-currency)         ; commodity
;;                       (gnc-numeric-zero) ; balance-num
;;                       0         ; depth
;;                       0         ; treedepth
;;                       #f         ; non-zero?
;;                       #f        ; summary?
;;                       (gnc:make-commodity-collector) ; subtotal-cc
;;                       #f        ;'()        ; sublist
;;                       ))

(define budget-record?                  (record-predicate <budget-record>))
(define budget-record-account           (record-accessor <budget-record> 'account))
(define budget-record-set-account!      (record-modifier <budget-record> 'account))
(define budget-record-code              (record-accessor <budget-record> 'code))
(define budget-record-set-code!         (record-modifier <budget-record> 'code))
(define budget-record-placeholder?      (record-accessor <budget-record> 'placeholder?))
(define budget-record-set-placeholder?! (record-modifier <budget-record> 'placeholder?))
(define budget-record-namelink          (record-accessor <budget-record> 'namelink))
(define budget-record-set-namelink!     (record-modifier <budget-record> 'namelink))
(define budget-record-commodity         (record-accessor <budget-record> 'commodity))
(define budget-record-set-commodity!    (record-modifier <budget-record> 'commodity))
(define budget-record-actual-cc         (record-accessor <budget-record> 'actual-cc))
(define budget-record-set-actual-cc!    (record-modifier <budget-record> 'actual-cc))
(define budget-record-budget-cc         (record-accessor <budget-record> 'budget-cc))
(define budget-record-set-budget-cc!    (record-modifier <budget-record> 'budget-cc))
(define budget-record-diff-cc           (record-accessor <budget-record> 'diff-cc))
(define budget-record-set-diff-cc!      (record-modifier <budget-record> 'diff-cc))

(define budget-record-actual-total-cc       (record-accessor <budget-record> 'actual-total-cc))
(define budget-record-set-actual-total-cc!  (record-modifier <budget-record> 'actual-total-cc))
(define budget-record-budget-total-cc       (record-accessor <budget-record> 'budget-total-cc))
(define budget-record-set-budget-total-cc!  (record-modifier <budget-record> 'budget-total-cc))
(define budget-record-diff-total-cc         (record-accessor <budget-record> 'diff-total-cc))
(define budget-record-set-diff-total-cc!    (record-modifier <budget-record> 'diff-total-cc))

(define budget-record-children          (record-accessor <budget-record> 'children))
(define budget-record-set-children!     (record-modifier <budget-record> 'children))


;; Sorts children account lists and passes INCOME or EXPENSE type accounts to their
;; respective build-*-record-list routines
;;(define (build-account-record-list account-list income-list expense-list)
;;  (cond
;;   ((null? account-list) '())
;;   (else (let ( (account (car account-list)) )
;;           (cond
;;            ;; If this is an income account...
;;            ((= (xaccAccountGetType account) ACCT-TYPE-INCOME)
;;             (let ()
;;               (build-income-account-record-list account income-list)))
;;            ((= (xaccAccountGetType account) ACCT-TYPE-EXPENSE)
;;             (let ()
;;               (build-expense-account-record-list account expense-list))))
;;           (build-account-record-list (cdr account-list) income-list expense-list))
;;         ))
;;  )




(define (build-expense-account-record-list root-account expense-list)
  (gnc:debug "DEVBGT: " (xaccAccountGetName root-account))
)


(define reportname (N_ "Development Budget"))

(define accounts-page    gnc:pagename-accounts)
;;(define commodities-page (N_ "Commodities"))
(define display-page     gnc:pagename-display)
(define general-page     gnc:pagename-general)
;;(define notes-page       (N_ "Notes"))

;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for your report's parameters.
(define (options-generator)
  (let* ((options (gnc:new-options))

         ;; This is just a helper function for making options.
         ;; See gnucash/src/scm/options.scm for details.
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-budget-option
      general-page
      (N_ "Budget")
      "a"
      (N_ "Budget for report")))

    (add-option
     (gnc:make-date-option
      general-page
      (N_ "Start Date")
      "ba"
      (N_ "Select which date to start generating the report from.")
      (lambda () (cons 'relative 'start-cal-year))
      #f
      'both
      '( today
         start-this-month
         start-prev-month
         start-current-quarter
         start-prev-quarter
         start-cal-year
         start-prev-year
         start-accounting-period)))

    (add-option
     (gnc:make-number-range-option
      general-page (N_ "Number of Report Periods")
      "bb" (N_ "Selects the number of periods to report from the start date.")
      12     ;; default
      0      ;; lower bound
      100000 ;; upper bound
      0      ;; number of decimals
      1      ;; step size
      ))

    (add-option
     (gnc:make-simple-boolean-option
      general-page (N_ "Generate Totals Column")
      "ca"
      (N_ "Selecting this adds a column at the end of the report that shows the individual account totals across the entire report time period.")
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      general-page (N_ "Generate YTD Totals Column")
      "cb"
      (N_ "Selecting this adds a column at the end of the report that shows the Year-to-Date totals for each account.")
      #f))

    (add-option
     (gnc:make-string-option
      display-page (N_ "Template file") "ce"
      (N_ "The file name of the eguile template part of this report.  This file must be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
      "dev-budget.eguile.scm"))

    (add-option
     (gnc:make-string-option
      display-page (N_ "CSS stylesheet file") "cf"
      (N_ "The file name of the CSS stylesheet to use with this report.  If specified, this file should be in your .gnucash directory, or else in its proper place within the GnuCash installation directories.")
      "dev-budget.css"))


;;    (add-option
;;     (gnc:make-date-option
;;      general-page
;;      (N_ "Account Totals Date")
;;      "da"
;;      (N_ "Select which date to use in generating the account totals column.")
;;      (lambda () (cons 'relative 'end-cal-year))
;;      #f
;;      'both
;;      '( today
;;         end-this-month
;;         end-prev-month
;;         end-current-quarter
;;         end-prev-quarter
;;         end-cal-year
;;         end-prev-year
;;         end-accounting-period)))


    ;; This is a color option, defined by rgba values. A color value
    ;; is a list where the elements are the red, green, blue, and
    ;; alpha channel values respectively. The penultimate argument
    ;; (255) is the allowed range of rgba values. The final argument
    ;; (#f) indicates the alpha value should be ignored. You can get
    ;; a color string from a color option with gnc:color-option->html,
    ;; which will scale the values appropriately according the range.
    (add-option
     (gnc:make-color-option
      display-page (N_ "Background Color")
      "a" (N_ "This is a color option")
      (list #xff #xff #xff 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      display-page (N_ "Text Color")
      "b" (N_ "This is a color option")
      (list #x00 #x00 #x00 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      display-page (N_ "Column Color 1")
      "c" (N_ "Select the color for one of the columns")
      (list #xFF #xFF #xFF 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      display-page (N_ "Column Color 2")
      "d" (N_ "Select the color for one of the columns")
      (list #xDD #xDD #xDD 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      display-page (N_ "Row Color 1")
      "e" (N_ "Select the color for one of the rows")
      (list #xFF #xFF #xFF 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      display-page (N_ "Row Color 2")
      "f" (N_ "Select the color for one of the rows")
      (list #xB3 #xDB #xFF 0)
      255
      #f))

    (add-option
     (gnc:make-string-option
      display-page
      (N_ "Font family") "g" (N_ "Font definition in CSS font-family format") "sans"))

    (add-option
     (gnc:make-string-option
      display-page
      (N_ "Font size") "h"
      (N_ "Font size in CSS font-size format (e.g. \"medium\" or \"10pt\"") "medium"))


    ;; Selection for accounts we want to report on
    (add-option
     (gnc:make-account-list-option
      ;;Which tab it goes under
      accounts-page
      ;;Name on the box in the tab
      (N_ "Select Accounts")
      ;;What order it has with other items in this tab
      "a"
      ;;What appears when hovered over by the mouse
      (N_ "Select accounts on which you want to report")
      ;;Default selection algorithm
      (lambda () '())
      ;;Which accounts to select
      #f
      ;;Are multiple selections allowed
      #t))

    (gnc:options-set-default-section options general-page)
    options))


;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation;; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (dev-detailed-budget-renderer report-obj)

  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))

  (define (op-value section name)
    (gnc:option-value (get-op section name)))



  ;; Find the start period in the budget that is the same month and year as the start date
  (define (get-start-period bdgt start-date)
    (let ((bdgt-start-date (car (gnc-budget-get-period-start-date bdgt 0)))
	  (period 0))
      (cond
       ((< start-date bdgt-start-date) (inexact->exact period))
       (else (inexact->exact (+ period (- (gnc:date-to-month-fraction start-date) (gnc:date-to-month-fraction bdgt-start-date))))))))

  (define (get-sumcol-period bdgt end-date)
    (let ((bdgt-start-date (car (gnc-budget-get-period-start-date bdgt 0)))
          (bdgt-end-date (car (gnc-budget-get-period-end-date bdgt 12)))
	  (period 0))
      (cond
       ((> end-date bdgt-end-date) (inexact->exact bdgt-end-date))
       (else (inexact->exact (+ period (- (gnc:date-to-month-fraction end-date) (gnc:date-to-month-fraction bdgt-start-date))))))))





;; HTML functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a row out of a title and a list of values
(define (build-html-row table values header? span-list color-list align-list row-base-color)
  (gnc:html-table-append-row! table (build-html-cell-list values header? span-list color-list align-list row-base-color)))


;; Creates a list of cells
(define (build-html-cell-list values header? span-list color-list align-list row-base-color)
  (let ((cell (gnc:make-html-table-cell)))
    ;;Keep going until there are no values left to generate
    (cond
     ((null? values) '())
     (else (cons (build-html-cell cell (car values) header? (car span-list) (car color-list) (car align-list) row-base-color)
                 (build-html-cell-list (cdr values) header? (cdr span-list) (cdr color-list) (cdr align-list) row-base-color))))))


;; Creates a cell with the value passed in
(define (build-html-cell cell value header? span color align row-base-color)

  ;; Start out by making the cell the first thing in the list
  (let ((attribute-list (list cell))
        (cell-color (get-cell-color-string color row-base-color))
        (pre-value "")
        (post-value ""))

    ;;Set the attribute list for a cell
    (set! attribute-list (append attribute-list (list "td")))

    ;;If it is a header row increase the font by 1
    (if (eqv? header? #t)
        (set! attribute-list (append attribute-list (list 'font-size "+1"))))

    ;;List all the attributes that are desired
    (set! attribute-list (append attribute-list (list 'attribute (list "align" align))))
    (set! attribute-list (append attribute-list (list 'attribute (list "nowrap" "nowrap"))))

    ;;Set the color to the color passed in
    (set! attribute-list (append attribute-list (list 'attribute (list "bgcolor" cell-color))))

    ;;If the number is negative set the text color to red
    (if (gnc:gnc-monetary? value)
        (if (gnc-numeric-negative-p (gnc:gnc-monetary-amount value))
            (set! attribute-list (append attribute-list (list 'font-color "#FF0000")))))

    ;;Set the column span
    (gnc:html-table-cell-set-colspan! cell span)

    ;;Apply all the attributes to the cell and then add the value to the cell
    (apply gnc:html-table-cell-set-style! attribute-list)
    (gnc:html-table-cell-append-objects! cell pre-value value post-value))

  ;;Return the cell
  cell)




;; Creates a row out of a title and a list of values
(define (build-html-budget-report-row table values header? bold? span-list color-list align-list row-base-color)
  (gnc:html-table-append-row! table (build-html-cell-list values header? bold? span-list color-list align-list row-base-color)))


;; Creates a list of cells
(define (build-html-budget-report-cell-list values header? bold? span-list color-list align-list row-base-color)
  (let ((cell (gnc:make-html-table-cell)))
    ;;Keep going until there are no values left to generate
    (cond
     ((null? values) '())
     (else (cons (build-html-cell cell (car values) header? bold? (car span-list) (car color-list) (car align-list) row-base-color)
                 (build-html-cell-list (cdr values) header? bold? (cdr span-list) (cdr color-list) (cdr align-list) row-base-color))))))


;; Creates a cell with the value passed in
(define (build-html-budget-report-cell cell value header? bold? span color align row-base-color)

  ;; Start out by making the cell the first thing in the list
  (let ((attribute-list (list cell))
        (cell-color (get-cell-color-string color row-base-color))
        (pre-value "")
        (post-value ""))

    ;;Set the attribute list for a cell
    (set! attribute-list (append attribute-list (list "td")))

    ;;If it is a header row increase the font by 1
    (if (eqv? header? #t)
        (set! attribute-list (append attribute-list (list 'font-size "+1"))))

    ;;If the values should be bolded that set the bold tags
    (if (eqv? bold? #t) (let ()
                         (set! pre-value "<b>")
                         (set! post-value "</b>")))

    ;;List all the attributes that are desired
    (set! attribute-list (append attribute-list (list 'attribute (list "align" align))))
    (set! attribute-list (append attribute-list (list 'attribute (list "nowrap" "nowrap"))))

    ;;Set the color to the color passed in
    (set! attribute-list (append attribute-list (list 'attribute (list "bgcolor" cell-color))))

    ;;If the number is negative set the text color to red
    (if (gnc:gnc-monetary? value)
        (if (gnc-numeric-negative-p (gnc:gnc-monetary-amount value))
            (set! attribute-list (append attribute-list (list 'font-color "#FF0000")))))

    ;;Set the column span
    (gnc:html-table-cell-set-colspan! cell span)

    ;;Apply all the attributes to the cell and then add the value to the cell
    (apply gnc:html-table-cell-set-style! attribute-list)
    (gnc:html-table-cell-append-objects! cell pre-value value post-value))

  ;;Return the cell
  cell)



  ;; Returns the a commodity collect for the budget, actual, and difference amount
;;  (define (budget-actual-difference-cc bdgt acnt period bdgt-cc act-cc diff-cc comm)
;;    (let* ((bdgt-val (gnc-budget-get-account-period-value bdgt acnt period))
;;           (act-val  (gnc-budget-get-account-period-actual-value bdgt acnt period))
;;           (bdgt-denom (gnc:gnc-numeric-denom bdgt-val))
;;           (act-denom  (gnc:gnc-numeric-denom act-val)))
;;
;;      ;; If the denom is not 100 make it 100
;;      (if (< bdgt-denom 100)
;;          (set! bdgt-val (gnc:make-gnc-numeric (* (gnc:gnc-numeric-num bdgt-val) (/ 100 bdgt-denom))
;;                                               100)))
;;
;;      (if (< act-denom 100)
;;          (set! act-val (gnc:make-gnc-numeric (* (gnc:gnc-numeric-num act-val) (/ 100 act-denom))
;;                                              100)))
;;
;;      ;; If it is an INCOME account negate the number because of the way it is stored
;;      (if (or (= (xaccAccountGetType acnt) ACCT-TYPE-INCOME) (= (xaccAccountGetType acnt) ACCT-TYPE-EQUITY))
;;          (set! act-val (gnc-numeric-neg act-val)))
;;
;;      ;;Create commodity collectors to be able to be able subtract the two values from each other for a diff
;;      (bdgt-cc 'add comm bdgt-val)
;;      (act-cc 'add comm act-val)
;;
;;      ;;Clear the diff-cc back to zero
;;      (diff-cc 'minusmerge diff-cc #f)
;;
;;      ;; If it is an INCOME account subtract the budget from the actual
;;      ;; Else it is an Expense account so subtract the actual from the budget
;;      (cond
;;       ((or (= (xaccAccountGetType acnt) ACCT-TYPE-INCOME) (= (xaccAccountGetType acnt) ACCT-TYPE-EQUITY))
;;        (let ()
;;          (diff-cc 'merge act-cc #f)
;;          (diff-cc 'minusmerge bdgt-cc #f)))
;;       (else
;;        (let ()
;;          (diff-cc 'merge bdgt-cc #f)
;;          (diff-cc 'minusmerge act-cc #f))))
;;      ))


  ;; Returns the a commodity collect for the budget, actual, and difference amount
  (define (budget-cc-list bdgt acnt start-period num-periods comm)
    (cond
     ( (= num-periods 0) '() )
     (else
      (let* ( (bdgt-cc  (gnc:make-commodity-collector))
              (bdgt-val (gnc-budget-get-account-period-value bdgt acnt start-period))
              (bdgt-denom (gnc:gnc-numeric-denom bdgt-val)))

        ;; If the denom is not 100 make it 100
        (if (< bdgt-denom 100)
            (set! bdgt-val (gnc:make-gnc-numeric (* (gnc:gnc-numeric-num bdgt-val) (/ 100 bdgt-denom))
                                                 100)))

        ;;Create commodity collectors to be able to be able subtract the two values from each other for a diff
        (bdgt-cc 'add comm bdgt-val)

        ;; return a list
        (cons bdgt-cc (budget-cc-list bdgt acnt (+ start-period 1) (- num-periods 1) comm))
        ))))


  ;; Returns the a commodity collect for the budget, actual, and difference amount
  (define (actual-cc-list bdgt acnt start-period num-periods comm)
    (cond
     ( (= num-periods 0) '() )
     (else
      (let* ( (act-cc   (gnc:make-commodity-collector))
              (act-val  (gnc-budget-get-account-period-actual-value bdgt acnt start-period))
              (act-denom  (gnc:gnc-numeric-denom act-val)))

        (if (< act-denom 100)
            (set! act-val (gnc:make-gnc-numeric (* (gnc:gnc-numeric-num act-val) (/ 100 act-denom))
                                                100)))

        ;; If it is an INCOME account negate the number because of the way it is stored
        (if (or (= (xaccAccountGetType acnt) ACCT-TYPE-INCOME) (= (xaccAccountGetType acnt) ACCT-TYPE-EQUITY))
            (set! act-val (gnc-numeric-neg act-val)))

        ;;Create commodity collectors to be able to be able subtract the two values from each other for a diff
        (act-cc 'add comm act-val)

        ;; return a list
        (cons act-cc (actual-cc-list bdgt acnt (+ start-period 1) (- num-periods 1) comm))
      ))))


  ;; Returns the a commodity collect for the budget, actual, and difference amount
  (define (difference-cc-list acnt bdgt-cc-list act-cc-list comm)
    (cond
     ((or (null? bdgt-cc-list) (null? act-cc-list) '()))
     (else
      (let* ( (diff-cc  (gnc:make-commodity-collector)) )

        ;;Clear the diff-cc back to zero
        (diff-cc 'minusmerge diff-cc #f)

        ;; If it is an INCOME account subtract the budget from the actual
        ;; Else it is an Expense account so subtract the actual from the budget
        (cond
         ((or (= (xaccAccountGetType acnt) ACCT-TYPE-INCOME) (= (xaccAccountGetType acnt) ACCT-TYPE-EQUITY))
          (let ()
            (diff-cc 'merge (car act-cc-list) #f)
            (diff-cc 'minusmerge (car bdgt-cc-list) #f)))
         (else
          (let ()
            (diff-cc 'merge (car bdgt-cc-list) #f)
            (diff-cc 'minusmerge (car act-cc-list) #f))))

        (cons diff-cc (difference-cc-list acnt (cdr bdgt-cc-list) (cdr act-cc-list) comm))
        ))))




  (define (build-account-record-list budget account-list start-period num-periods)
    (cond
     ((null? account-list) '())
     (else (let* ( (new-record (blank-budget-record))
                   (account (car account-list))
                   (comm (xaccAccountGetCommodity account))
                   (children (gnc-account-get-children account))
                   (budget-total (gnc:make-commodity-collector))
                   (actual-total (gnc:make-commodity-collector))
                   (diff-total (gnc:make-commodity-collector))
                   (children-list '())
                   (sum-list '()) )
             (budget-record-set-account!      new-record account)
             (budget-record-set-code!         new-record (xaccAccountGetCode account))
             (budget-record-set-placeholder?! new-record (xaccAccountGetPlaceholder account))
             (budget-record-set-namelink!     new-record (account-link account))
             (budget-record-set-commodity!    new-record comm)
             (budget-record-set-children!     new-record (build-account-record-list budget children start-period num-periods))


             (budget-record-set-actual-cc!    new-record (actual-cc-list budget account start-period num-periods comm))
             (budget-record-set-budget-cc!    new-record (budget-cc-list budget account start-period num-periods comm))
             (gnc:debug "DEVBGT: Actual list has value: " (cc-list-nonzero? (budget-record-actual-cc  new-record)))
             (gnc:debug "DEVBGT: Budget list has value: " (cc-list-nonzero? (budget-record-budget-cc  new-record)))

             (if (and (not (cc-list-nonzero? (budget-record-budget-cc new-record)))
                      (budget-record-placeholder? new-record))
                 (budget-record-set-budget-cc! new-record
                                               (sum-budget-list-for-budget-record-list (budget-record-children new-record))) )


;; why did I need children list?
;;             (set! children-list (budget-record-children new-record))

;;             This call worked.
;;             (set! sum-list (sum-budget-list-for-budget-record-list (budget-record-children new-record)))

;; I don't remember what this line was testing
;;             (set! sum-list (sum-cc-lists (budget-record-budget-cc new-record) (budget-record-actual-cc new-record)))

             (budget-record-set-diff-cc!      new-record (difference-cc-list account (budget-record-budget-cc new-record) (budget-record-actual-cc new-record) comm))

             ;; Make totals
             (actual-total 'merge (total-cc-list (budget-record-actual-cc  new-record)) #f)
             (budget-total 'merge (total-cc-list (budget-record-budget-cc  new-record)) #f)
             (diff-total 'merge (total-cc-list (budget-record-diff-cc  new-record)) #f)
             ;;(gnc:debug "DEVBGT: Budget total: " (budget-total 'getmonetary comm #f))

             (budget-record-set-actual-total-cc!  new-record actual-total)
             (budget-record-set-budget-total-cc!  new-record budget-total)
             (budget-record-set-diff-total-cc!    new-record diff-total)

             (cons new-record (build-account-record-list budget (cdr account-list) start-period num-periods)) )
         ) ;; else
   ) ;; cond
  )


;;  (define (is-cc-list-zero cc-list comm)
;;    (cond
;;     ( (null? cc-list) #t)
;;     (else (let* ( (cc (car cc-list))
;;                   (val (gnc:gnc-monetary-amount (cc 'getmonetary comm #f))) )
;;             (cond
;;              ( (gnc-numeric-zero-p val) #f )
;;              (else (is-cc-list-zero (cdr cc-list comm))))
;;             ))
;;     ))
;;
;;
;;  (define (sum-children-budget-cc-list children-record-list)
;;    (let ( (budget-cc-list (budget-record-budget-cc (car children-record-list)))
;;           (summed-budget-cc-list (sum-children-budget-cc-list (cdr children-record-list))) )
;;      (cond
;;      ))
;;
;;
;;  (define (sum-cc-list first-list second-list)
;;    (cond
;;     ( (or (null? first-list) (null? second-list)) '())
;;     (else (let ( (first (car first-list))
;;                  (second (car second-list)) )
;;             (cons (first 'merge second #f) (sum-cc-list (cdr first-list) (cdr second-list)))
;;             ))


(define (to-monetary-list cc-list comm)
  (cond
   ( (null? cc-list) '() )
   (else (let ( (cc (car cc-list)) )
           (cons (cc 'getmonetary comm #f) (to-monetary-list (cdr cc-list) comm)))
         )))

(define (total-cc-list cc-list)
  (cond
   ( (null? cc-list) (gnc:make-commodity-collector) )
   (else (let ( (cc (total-cc-list (cdr cc-list))) )
           (cc 'merge (car cc-list) #f)
           cc))))

;; returns #f of all collectors and all entries are zero value.
(define (cc-list-nonzero? cc-list)
  (let ( (result #t) )
    (cond
     ( (null? cc-list) result )
     (else (set! result (and (gnc-commodity-collector-allzero? (car cc-list)) (cc-list-nonzero? (cdr cc-list)))) ))
    result))


(define (sum-budget-list-for-budget-record-list record-list)
  (cond
   ( (null? record-list) '() )
   (else
    (let* ( (current-record (car record-list))
            (current-budget-list (budget-record-budget-cc current-record))
            (result-list (budget-record-budget-cc current-record)) )
      (gnc:debug "DEVBGT: sum-budget name: " (xaccAccountGetName (budget-record-account current-record)))
      (gnc:debug "  sum-budget list length: " (length (budget-record-budget-cc current-record)))
      (gnc:debug "  sum-budget record-list length: " (length record-list))
      (gnc:debug "  sum-budget cdr record-list length: " (length (cdr record-list)))
      (gnc:debug "  sum-budget cdr record-list null?: " (not (null? (cdr record-list))))
      (if (not (null? (cdr record-list)))
          (set! result-list (sum-cc-lists current-budget-list (sum-budget-list-for-budget-record-list (cdr record-list))))
          (gnc:debug "  sum-cc-lists call not executed"))
      (gnc:debug "  sum-budget return  " (xaccAccountGetName (budget-record-account current-record)))
      (gnc:debug "  sum-budget result length: " (length result-list))
      result-list))))

;;      (cond
;;       ( (null? (cdr record-list)) current-budget-list )
;;       (else (sum-cc-lists current-budget-list (sum-budget-list-for-budget-record-list (cdr record-list)))))
;;      ))))


(define (sum-cc-lists first-list second-list)
  (cond
   ((or (null? first-list) (null? second-list))
    (gnc:debug "1 sum-cc-lists: One of the lists is null.")
    '())
   ((or (null? (cdr first-list)) (null? (cdr second-list)))
    (let ( (first (car first-list))
           (second (car second-list)) )
      (gnc:debug "2 First length " (length first-list) "    Second length " (length second-list))
      (gnc:debug "2 First " first  "    Second length " second)
      (first 'merge second #f)
      (list first)))
   (else
    (let ( (first (car first-list))
           (second (car second-list)) )
      (gnc:debug "3 First length " (length first-list) "    Second length " (length second-list))
      (gnc:debug "3 First " first  "    Second length " second)
      (first 'merge second #f)
      (cons first (sum-cc-lists (cdr first-list) (cdr second-list)))))))



  ;; This is the main routine that puts it all together
  (let* ((document (gnc:make-html-document))
         (table (gnc:make-html-table))
         ;;(income-accounts '())
         ;;(expense-accounts '())
         (budget (op-value general-page "Budget"))
         (num-periods (op-value general-page "Number of Report Periods"))
         (start-period (get-start-period budget (car (gnc:date-option-absolute-time (op-value general-page "Start Date")))))
         (current-period (get-start-period budget (car (gnc:get-start-this-month))))
         ;;(debug-port (open-output-file "budget-debug.txt"))
         (debug-port (open-output-string))
         (income-accounts (build-account-record-list budget (assoc-ref (gnc:decompose-accountlist (gnc-account-get-children (gnc-get-current-root-account)))  ACCT-TYPE-INCOME) start-period num-periods))
         (expense-accounts (build-account-record-list budget (assoc-ref (gnc:decompose-accountlist (gnc-account-get-children (gnc-get-current-root-account)))  ACCT-TYPE-EXPENSE) start-period num-periods))

         (opt-font-family      (get-op display-page     (N_ "Font family")))
         (opt-font-size        (get-op display-page     (N_ "Font size")))
         (opt-css-file         (get-op display-page     (N_ "CSS stylesheet file")))

         (css? (and (defined? 'gnc-html-engine-supports-css) (gnc-html-engine-supports-css)))
         (html #f))

    (set! html (eguile-file-to-string
                (find-template "dev-budget.eguile.scm")
                (the-environment)))
    (gnc:debug "dev-budget.scm - generated html:") (gnc:debug html)
    (gnc:html-document-add-object!
     document
     html)





;; Pre-eguile document
;; -----------------------------------------------------------------------------

         ;;Setup the Title and font color and back ground color of the document
;;         (gnc:html-document-set-title! document reportname)
;;         (gnc:html-document-set-style! document "body"
;;                                       'attribute (list "bgcolor" (gnc:color-option->html (get-op "Color" "Background Color")))
;;                                       'font-color (gnc:color-option->html (get-op "Color" "Text Color")))



;; Saved for reference

;;    (gnc:html-document-add-object!
;;     document
;;     (gnc:make-html-text
;;      (gnc:html-markup-p
;;       (gnc:html-markup/format
;;        (_ "The value is %d.")
;;        (gnc:date-to-month-fraction end-date)
;;        ;;(gnc:html-markup-b sumcol-period)))
;;      ))
    ;;(gnc:debug "Sum column period.. " (gnc:date-get-month-year-string (localtime (car (gnc:date-option-absolute-time (op-value "General" "Account Totals Date"))))))
    ;;(gnc:debug "End period.. " (gnc:date-get-month-year-string (gnc:timepair->date (gnc-timespec2timepair (gnc-budget-get-period-end-date budget 0)))))
;;    (budget-record-printer brec debug-port)
;;    (gnc:debug "String port" (get-output-string debug-port))








;; Main debug code

;;    (budget-record-list-printer income-accounts 1 debug-port)
;;    (budget-record-list-printer expense-accounts 1 debug-port)
;;
;;
;;    (gnc:html-document-add-object!
;;     document
;;     (gnc:make-html-text
;;      (gnc:html-markup-p
;;       (gnc:html-markup-b (_ "Record output:")))
;;      (gnc:html-markup-tt
;;       (get-output-string debug-port))
;;        ;;(gnc:html-markup-b sumcol-period)))
;;      ))
;;    (close-output-port debug-port)




    ;;Add the generated table to the document
;;    (gnc:html-document-add-object! document table)


;; -----------------------------------------------------------------------------
;; End pre-eguile


    ;;Return the document
    document)
  )



;; Here we define the actual report with gnc:define-report
(gnc:define-report

 ;; The version of this report.
 'version 1

 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name reportname

 ;; GUID for this report
 'report-guid "e94f60f75fe644c2919c877e9a21a2fe"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name (N_ reportname)

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "A detailed budget report displaying budget totals and balance.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-income-expense)

 ;; The options generator function defined above.
 'options-generator options-generator

 ;; The rendering function defined above.
 'renderer dev-detailed-budget-renderer)
