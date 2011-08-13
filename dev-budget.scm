;; -*-scheme-*-

(define-module (gnucash report user-reports dev-budget))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;;for gnc-build-url
;;(gnc:module-load "gnucash/report/user-reports/budget-utils" 0)
(use-modules (gnucash report user-reports budget-utils))

(define reportname (N_ "Development Budget"))

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
      (N_ "General")
      (N_ "Budget")
      "a"
      (N_ "Budget for report")))

    (add-option
     (gnc:make-date-option
      (N_ "General")
      (N_ "Start Date")
      "ba"
      (N_ "Select which date to start generating the report from.")
      (lambda () (cons 'relative 'start-cal-year))
      #f
      'both
      '(
	today
	start-this-month
	start-prev-month
	start-current-quarter
	start-prev-quarter
	start-cal-year
	start-prev-year
	start-accounting-period)))

    (add-option
     (gnc:make-number-range-option
      (N_ "General") (N_ "Number of Report Periods")
      "bb" (N_ "Selects the number of periods to report from the start date.")
      12     ;; default
      0      ;; lower bound
      100000 ;; upper bound
      0      ;; number of decimals
      1      ;; step size
      ))

    (add-option
     (gnc:make-simple-boolean-option
      (N_ "General") (N_ "Generate Year to Date (YTD) column.")
      "c"
      (N_ "Selecting this adds a column at the end of the report that is a YTD.")
      #t))

    (add-option
     (gnc:make-simple-boolean-option
      (N_ "General") (N_ "Generate End of Year (EOY) column.")
      "da"
      (N_ "Selecting this adds a column at the end of the report that uses the date below to create a column of year totals.")
      #f))

    (add-option
     (gnc:make-date-option
      (N_ "General")
      (N_ "End of Year (EOY) Date")
      "db"
      (N_ "Select which date to generating the EOY column for. ONLY THE YEAR IS USED.")
      (lambda () (cons 'relative 'start-cal-year))
      #f
      'both
      '(
	today
	start-this-month
	start-prev-month
	start-current-quarter
	start-prev-quarter
	start-cal-year
	start-prev-year
	start-accounting-period)))

    (add-option
     (gnc:make-simple-boolean-option
      (N_ "General") (N_ "Include accounts with only actual values.")
      "dc"
      (N_ "Selecting this adds a column at the end of the report that is a YTD.")
      #t))


    ;; This is a color option, defined by rgba values. A color value
    ;; is a list where the elements are the red, green, blue, and
    ;; alpha channel values respectively. The penultimate argument
    ;; (255) is the allowed range of rgba values. The final argument
    ;; (#f) indicates the alpha value should be ignored. You can get
    ;; a color string from a color option with gnc:color-option->html,
    ;; which will scale the values appropriately according the range.
    (add-option
     (gnc:make-color-option
      (N_ "Color") (N_ "Background Color")
      "a" (N_ "This is a color option")
      (list #xff #xff #xff 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      (N_ "Color") (N_ "Text Color")
      "b" (N_ "This is a color option")
      (list #x00 #x00 #x00 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      (N_ "Color") (N_ "Column Color 1")
      "c" (N_ "Select the color for one of the columns")
      (list #xFF #xFF #xFF 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      (N_ "Color") (N_ "Column Color 2")
      "d" (N_ "Select the color for one of the columns")
      (list #xDD #xDD #xDD 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      (N_ "Color") (N_ "Row Color 1")
      "e" (N_ "Select the color for one of the rows")
      (list #xFF #xFF #xFF 0)
      255
      #f))

    (add-option
     (gnc:make-color-option
      (N_ "Color") (N_ "Row Color 2")
      "f" (N_ "Select the color for one of the rows")
      (list #xB3 #xDB #xFF 0)
      255
      #f))

    ;; Selection for accounts we want to report on
    (add-option
     (gnc:make-account-list-option
      ;;Which tab it goes under
      (N_ "Accounts")
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

    (gnc:options-set-default-section options "General")
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

  ;; Find the first period for the End of Year date given
  (define (get-eoy-start-period bdgt eoy-date period)
    (let ((bdgt-start-date (car (gnc-budget-get-period-start-date bdgt period))))
      (cond
       ((= (gnc:date-get-year (localtime eoy-date)) (gnc:date-get-year (localtime bdgt-start-date))) (inexact->exact period))
       (else (get-eoy-start-period bdgt eoy-date (+ period 1))))))

  ;; Find the number of periods for the End of Year date given
  (define (get-eoy-end-period bdgt eoy-date period)
    (let ((bdgt-start-date (car (gnc-budget-get-period-start-date bdgt period))))
      (cond
       ((= (gnc:date-get-year (localtime eoy-date)) (gnc:date-get-year (localtime bdgt-start-date)))
	(get-eoy-end-period bdgt eoy-date (+ period 1)))
       (else (inexact->exact (- period 1))))))

  ;; Returns a list of dates for the budget
  (define (budget-date-list bdgt period num-periods)
    (cond
     ((= num-periods 0) (let ((eoy-string (string-append "End Of Year (" (gnc:date-get-year-string (localtime (car (gnc:date-option-absolute-time (op-value "General" "End of Year (EOY) Date"))))) ")")))
			  (if (op-value "General" "Generate Year to Date (YTD) column.")
			      (if (op-value "General" "Generate End of Year (EOY) column.")
				  (list "Year To Date" eoy-string)
				  '("Year To Date"))
			      (if (op-value "General" "Generate End of Year (EOY) column.")
				  (list eoy-string)
				  '()))))
     (else (cons (gnc:date-get-month-year-string (gnc:timepair->date (gnc-timespec2timepair (gnc-budget-get-period-start-date bdgt (inexact->exact period)))))
		 (budget-date-list bdgt (+ period 1) (- num-periods 1))))))

  ;; Creates a row out of a title and a list of values
  (define (build-html-row header bold span-list table values color-list align-list row-base-color)
    (gnc:html-table-append-row! table (build-html-cell-list header bold span-list values color-list align-list row-base-color)))

  ;; Creates a list of cells
  (define (build-html-cell-list header bold span-list values color-list align-list row-base-color)
    (let ((cell (gnc:make-html-table-cell)))
      ;;Keep going until there are no values left to generate
      (cond
       ((null? values) '())
       (else (cons (build-html-cell header bold (car span-list) cell (car values) (car color-list) (car align-list) row-base-color)
		   (build-html-cell-list header bold (cdr span-list) (cdr values) (cdr color-list) (cdr align-list) row-base-color))))))

  ;; Turns a number into a hex-string
  (define (number->hex-string number)
    (number->string number 16))

  ;; Turns a hex-string into a number
  (define (hex-string->number hex-string)
    (string->number hex-string 16))

  ;; Returns a hex string of 2 characters if it is over 255
  ;;  then it returns ff otherwise it returns the value of the number in a hex-string
  (define (single-color-string num base-num)
    (let ((sum-string (number->hex-string (- base-num (- 255 num)))))
      (cond
       ((= (string-length sum-string) 2) sum-string)
       ((< (string-length sum-string) 2) (string-append "0" sum-string))
       (else (number->hex-string 0)))))

  ;; Returns a hex string with a # and the beginning
  ;;  Calculate each color independently so overflow from one doesn't mix into the next
  (define (get-cell-color-string color base-color)
    (string-append "#"
		   (single-color-string (hex-string->number (substring color 0 2)) (hex-string->number (substring base-color 0 2)))
		   (single-color-string (hex-string->number (substring color 2 4)) (hex-string->number (substring base-color 2 4)))
		   (single-color-string (hex-string->number (substring color 4 6)) (hex-string->number (substring base-color 4 6)))))

  ;; Creates a cell with the value passed in
  (define (build-html-cell header bold span cell value color align row-base-color)

    ;; Start out by making the cell the first thing in the list
    (let ((attribute-list (list cell))
	  (cell-color (get-cell-color-string color row-base-color))
	  (pre-value "")
	  (post-value ""))

      ;;Set the attribute list for a cell
      (set! attribute-list (append attribute-list (list "td")))

      ;;If it is a header row increase the font by 1
      (if (eqv? header #t)
	  (set! attribute-list (append attribute-list (list 'font-size "+1"))))

      ;;If the values should be bolded that set the bold tags
      (if (eqv? bold #t) (let ()
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

  ;; Returns the Year to Date budget account and difference value
  (define (ytd-budget-act-diff bdgt acnt start-period curr-period bdgt-cc act-cc diff-cc comm)
    (cond
     ((> start-period curr-period) (list (bdgt-cc 'getmonetary comm #f) (act-cc 'getmonetary comm #f) (diff-cc 'getmonetary comm #f)))
     (else (let ()
	     (budget-act-diff-cc bdgt acnt start-period bdgt-cc act-cc diff-cc comm)
	     (ytd-budget-act-diff bdgt acnt (+ start-period 1) curr-period bdgt-cc act-cc diff-cc comm)))))

  ;; Returns the End of Year budget account and difference value
  (define (eoy-budget-act-diff bdgt acnt start-period end-period bdgt-cc act-cc diff-cc comm)
    (cond
     ((> start-period end-period) (list (bdgt-cc 'getmonetary comm #f) (act-cc 'getmonetary comm #f) (diff-cc 'getmonetary comm #f)))
     (else (let ()
	     (budget-act-diff-cc bdgt acnt start-period bdgt-cc act-cc diff-cc comm)
	     (eoy-budget-act-diff bdgt acnt (+ start-period 1) end-period bdgt-cc act-cc diff-cc comm)))))

  ;; Returns the a commodity collect for the budget actual and difference amount
  (define (budget-act-diff-cc bdgt acnt period bdgt-cc act-cc diff-cc comm)
    (let* ((bdgt-val (gnc-budget-get-account-period-value bdgt acnt period))
	   (act-val  (gnc-budget-get-account-period-actual-value bdgt acnt period))
	   (bdgt-denom (gnc:gnc-numeric-denom bdgt-val))
	   (act-denom  (gnc:gnc-numeric-denom act-val)))

      ;; If the denom is not 100 make it 100
      (if (< bdgt-denom 100)
	  (set! bdgt-val (gnc:make-gnc-numeric (* (gnc:gnc-numeric-num bdgt-val) (/ 100 bdgt-denom))
					       100)))

      (if (< act-denom 100)
	  (set! act-val (gnc:make-gnc-numeric (* (gnc:gnc-numeric-num act-val) (/ 100 act-denom))
					       100)))

      ;; If it is an INCOME account negate the number because of the way it is stored
      (if (or (= (xaccAccountGetType acnt) ACCT-TYPE-INCOME) (= (xaccAccountGetType acnt) ACCT-TYPE-EQUITY))
	  (set! act-val (gnc-numeric-neg act-val)))

      ;;Create commodity collectors to be able to be able subtract the two values from each other for a diff
      (bdgt-cc 'add comm bdgt-val)
      (act-cc 'add comm act-val)

      ;;Clear the diff-cc back to zero
      (diff-cc 'minusmerge diff-cc #f)

      ;; If it is an INCOME account subtract the budget from the actual
      ;; Else it is an Expense account so subtract the actual from the budget
      (cond
       ((or (= (xaccAccountGetType acnt) ACCT-TYPE-INCOME) (= (xaccAccountGetType acnt) ACCT-TYPE-EQUITY))
	(let ()
	  (diff-cc 'merge act-cc #f)
	  (diff-cc 'minusmerge bdgt-cc #f)))
       (else
	(let ()
	  (diff-cc 'merge bdgt-cc #f)
	  (diff-cc 'minusmerge act-cc #f))))))

  ;; Returns a list that has a budget values, account values and the difference for all the periods
  (define (account-budget-act-diff-row bdgt acnt start-period num-periods)
    (let ((comm (xaccAccountGetCommodity acnt))
	  (bdgt-cc  (gnc:make-commodity-collector))
	  (act-cc   (gnc:make-commodity-collector))
	  (diff-cc  (gnc:make-commodity-collector)))

      ;;If the number of periods is 0 stop processing
      ;; When returning the commodity values just return a value with the commodity symbol in from
      (cond
       ((= num-periods 0) (let* ((ytd-start-period (get-start-period bdgt (car (gnc:get-start-cal-year))))
				 (ytd-curr-period  (get-start-period bdgt (car (gnc:get-start-this-month))))
				 (eoy-start-period
				  (get-eoy-start-period bdgt (car (gnc:date-option-absolute-time (op-value "General" "End of Year (EOY) Date"))) 0))
				 (eoy-end-period
				  (get-eoy-end-period bdgt (car (gnc:date-option-absolute-time (op-value "General" "End of Year (EOY) Date"))) eoy-start-period)))
			    (if (op-value "General" "Generate Year to Date (YTD) column.")
				(if(op-value "General" "Generate End of Year (EOY) column.")
				   (append (ytd-budget-act-diff bdgt
								acnt
								ytd-start-period
								ytd-curr-period
								bdgt-cc
								act-cc
								diff-cc
								comm)
					   (eoy-budget-act-diff bdgt
								acnt
								eoy-start-period
								eoy-end-period
								bdgt-cc
								act-cc
								diff-cc
								comm))
				   (ytd-budget-act-diff bdgt
							acnt
							ytd-start-period
							ytd-curr-period
							bdgt-cc
							act-cc
							diff-cc
							comm))
				(if(op-value "General" "Generate End of Year (EOY) column.")
				    (eoy-budget-act-diff bdgt
							 acnt
							 eoy-start-period
							 eoy-end-period
							 bdgt-cc
							 act-cc
							 diff-cc
							 comm)
				    '()))))
       (else (let ((bdgt-val (gnc-budget-get-account-period-value bdgt acnt start-period))
		   (act-val  (gnc-budget-get-account-period-actual-value bdgt acnt start-period)))

	       (budget-act-diff-cc bdgt acnt start-period bdgt-cc act-cc diff-cc comm)

	       ;;Return the budget actual and differential amount
	       (append (list (bdgt-cc 'getmonetary comm #f)
			     (act-cc 'getmonetary comm #f)
			     (diff-cc 'getmonetary comm #f))
		       (account-budget-act-diff-row bdgt acnt (+ start-period 1) (- num-periods 1))))))))



  ;; Adds values in the account list of values to the totals list
  (define (add-acnt-vals-to-totals acnt-values totals-list)
    (cond
     ((null? acnt-values) '())
     (else (let* ((acnt-val  (gnc:gnc-monetary-amount (car acnt-values)))
		  (total-val (gnc:gnc-monetary-amount (car totals-list)))
		  (total-cc  (gnc:make-commodity-collector))
		  (comm      (gnc:gnc-monetary-commodity (car acnt-values))))
	     (total-cc 'add comm total-val)
	     (total-cc 'add comm acnt-val)
	     (cons (total-cc 'getmonetary comm #f) (add-acnt-vals-to-totals (cdr acnt-values) (cdr totals-list)))))))

  ;; Adds a row for each account in the account list
  ;;  with a budget value, real account value and difference for every period
  (define (build-value-html-table table acnt-list bdgt start-period num-periods color-list align-list totals-list row-color-1 row-color-2)
    (let ((row-color (if (odd? (length acnt-list))
			 row-color-1
			 row-color-2)))

      ;;When the account list is empty stop
      ;;Otherwise add another row starting with account name and call the function again
      (cond
       ((null? acnt-list) (let ()
			    ;;Build the Totals Row
			    (build-html-row #f #t (gen-list (+ (length totals-list) 1) 1) table (cons "Totals" totals-list) color-list align-list row-color)
			    totals-list))
       (else (let* ((acnt (car acnt-list))
		    (acnt-values (account-budget-act-diff-row bdgt acnt start-period num-periods)) ;Generates a list of values of the account
		    (temp-totals-list (add-acnt-vals-to-totals acnt-values totals-list))           ;Adds the values to the total values
		    (acnt-name-values (cons (gnc:html-account-anchor acnt) acnt-values)))          ;Adds the account name to the beginning of the account value list
	       (build-html-row #f #f (gen-list (length acnt-name-values) 1) table acnt-name-values color-list align-list row-color)
	       (build-value-html-table table (cdr acnt-list) bdgt start-period num-periods color-list align-list temp-totals-list row-color-1 row-color-2))))))

  ;; Span List for Date header
  ;;  The first cell is 1 and all the rest are 3
  ;;  The first one is for account name the rest or for periods
  (define (date-header-span-list num-periods)
    (cons '1 (gen-list num-periods 3)))

  ;; Generate a list of the lenght of num-times and of the value passed in
  (define (gen-list num-times value)
    (cond
     ((= num-times 0) '())
     (else (cons value (gen-list (- num-times 1) value)))))

  ;; Budget Actual Difference header builder
  ;;  This builds a list with Budget Actual and Difference
  ;;  repeated for the number of periods passed in
  (define (budget-actual-diff-header num-periods)
    (cond
     ((= num-periods 0) '())
     (else (append (list 'Budget 'Actual 'Diff) (budget-actual-diff-header (- num-periods 1))))))

  ;; Build a color list for the date table
  (define (date-header-color-list num-periods)
    (cond
     ((= num-periods 0) '())
     ((odd? (inexact->exact num-periods)) (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 2")) (date-header-color-list (- num-periods 1))))
     ((even? (inexact->exact num-periods)) (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1")) (date-header-color-list (- num-periods 1))))))

  ;; Build a list with 3 one color and 3 another color alternating every 3
  (define (three-on-three-off-color num-periods)
    (let ((color_1 (gnc:color-option->hex-string (get-op "Color" "Column Color 2")))
	  (color_2 (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))))
    (cond
     ((= num-periods 0) '())
     ((odd? (inexact->exact num-periods)) (append (three-on-three-off-color (- num-periods 1))
						  (list color_1 color_1 color_1)))
     ((even? (inexact->exact num-periods)) (append (three-on-three-off-color (- num-periods 1))
						   (list color_2 color_2 color_2))))))

  ;; Build a header alignment list
  (define (header-align-list num-periods)
    (cond
     ((= num-periods 0) '())
     (else (cons 'center (header-align-list (- num-periods 1))))))

  ;; Build an account values alignment list
  (define (account-values-align-list num-periods)
    (cond
     ((= num-periods 0) '())
     (else (cons 'right (account-values-align-list (- num-periods 1))))))

  ;; Generates a list of gnc-monetary for collecting totals
  (define (gen-list-monetary num-comm acnt)
    (cond
     ((= num-comm 0))
     (else (let* ((comm (xaccAccountGetCommodity acnt))
		  (temp-cc (gnc:make-commodity-collector)));
	     (temp-cc 'add comm (gnc:make-gnc-numeric 0 100))
	     (cons (temp-cc 'getmonetary comm #f) (gen-list-monetary (- num-comm 1) acnt))))))

  ;; Generates a row of cells that creates line
  (define (gen-horiz-line num-cells table)
    (gnc:html-table-append-row! table (gen-horiz-line-cell-list num-cells)))

  ;; Generates a list of cells that create a horizonal line
  (define (gen-horiz-line-cell-list num-cells)
    (cond
     ((= num-cells 0) '())
     (else (cons (gen-horiz-line-cell num-cells) (gen-horiz-line-cell-list (- num-cells 1))))))


  ;; Generate a list of cells
  (define (gen-horiz-line-cell num-cells)
    (let* ((cell (gnc:make-html-table-cell))
	   (attribute-list (list cell))
	   (pre-html-value "<b><sub>")
	   (post-html-value "</sub></b>")
	   (html-value ""))

      (set! attribute-list (append attribute-list (list "td")))

      ;;Set the color to BLACK
      (set! attribute-list (append attribute-list (list 'attribute (list "bgcolor" "#FFFFFF"))))

      ;;Decrease the font size to make the row thinner
      (set! attribute-list (append attribute-list (list 'font-size "1")))

      ;;Apply all the attributes to the cell and then add the value to the cell
      (apply gnc:html-table-cell-set-style! attribute-list)

      ;;Set the column span
;      (gnc:html-table-cell-set-colspan! cell num-cells)

      ;;Add an horizonal line to the cell
      ;; This is a hack.  GnuCash doesn't let you put nothing in a cell.
      ;; If left empty GnCash puts in a <BR> tag which causes the cell to
      ;; be the height of the font.  If it could be left empty then the
      ;  height= could be used in the HTML to allw the cell to be made
      ;; thin but since the cell can't be left blank we put in a "." and
      ;; superscript it as small as it will go.
      (gnc:html-table-cell-append-objects! cell pre-html-value html-value post-html-value)

    ;;Return the cell
    cell))

  ;; Return and list that is a diff of the income and expense lists passed in
  ;; Pass in the col-num because every 3 column is a diff column and has to be treated differently
  (define (diff-income-expense-list income-list expense-list col-num)
    (cond
     ((null? income-list) '())
     (else (let* ((income-val  (cond ;;Every third column is a diff row and the income diff has had its value inverted so we want to undo that invert on the diff columns
				((= col-num 3)
				 (set! col-num 0)
				 (gnc:gnc-monetary-amount (gnc:monetary-neg (car income-list))))
				(else
				 (gnc:gnc-monetary-amount (car income-list)))))
		  (expense-val (gnc:gnc-monetary-amount (gnc:monetary-neg (car expense-list)))) ;;Make the expense negative so when it adds it actually subtracts
		  (diff-cc     (gnc:make-commodity-collector))
		  (comm        (gnc:gnc-monetary-commodity (car income-list))))

	     (diff-cc 'add comm income-val)
	     (diff-cc 'add comm expense-val) ;;Expense was made negative early so this is actually a subtract
	     (cons (diff-cc 'getmonetary comm #f) (diff-income-expense-list (cdr income-list) (cdr expense-list) (+ col-num 1)))))))

  ;; This adds the Date, Budget Actual and Diff header to all the columns
  (define (date-bdgt-act-diff-header-table table budget num-col start-period num-periods)
    ;;Build the Date Header Row
    (build-html-row #t
		    #t
		    (date-header-span-list (+ num-col 1))
		    table
		    (cons "Accounts" (budget-date-list budget start-period num-periods))
		    (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))
			  (reverse (date-header-color-list num-col)))
		    (header-align-list (+ num-col 1))
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1")))

    ;;Build the Budget Actual Diff Header Row
    (build-html-row #f
		    #f
		    (gen-list (+ (* num-col 3) 1) 1)
		    table
		    (cons " " (budget-actual-diff-header num-col))
		    (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))
			  (three-on-three-off-color num-col))
		    (header-align-list (+ (* num-col 3) 1))
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1"))))

  ;; Generates part of the HTML table for a specific Account Catagory (ie Expense, Income)
  (define (build-catagory-table table acnt-list catagory-name budget start-period num-periods num-col)
    ;;Build Catagory Header
    (build-html-row #t
		    #t
		    (gen-list (+ (* num-col 3) 1) 1)
		    table
		    (cons catagory-name (gen-list (* num-col 3) " "))
		    (gen-list (+ (* num-col 3) 1)  (gnc:color-option->hex-string (get-op "Color" "Column Color 1")))
		    (gen-list (+ (* num-col 3) 1) 'left)
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1")))

    ;;Put in the header for all the coloums
    (date-bdgt-act-diff-header-table table budget num-col start-period num-periods)

    ;;Build the rest of the table (This returns the totals array so make sure it is last or sets a variable that is returned)
    (build-value-html-table table
			    acnt-list
			    budget
			    start-period
			    num-periods
			    (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))
				  (three-on-three-off-color num-col))
			    (cons 'left (account-values-align-list (* num-col 3)))
			    (gen-list-monetary (* num-col 3) (car acnt-list))
			    (if (odd? (length acnt-list))
				(gnc:color-option->hex-string (get-op "Color" "Row Color 2"))  ;;This ensures that the first row always starts with the same color
				(gnc:color-option->hex-string (get-op "Color" "Row Color 1")))
			    (if (odd? (length acnt-list))
				(gnc:color-option->hex-string (get-op "Color" "Row Color 1"))  ;;This ensures that the first row always starts with the same color
				(gnc:color-option->hex-string (get-op "Color" "Row Color 2")))))

  ;; Add the totals at to the html table
  (define (build-totals-table table budget num-col start-period num-periods income-totals-list expense-totals-list diff-totals-list)
    ;;Put in the Totals header
    (build-html-row #t
		    #t
		    (gen-list (+ (* num-col 3) 1) 1)
		    table
		    (cons "Totals" (gen-list (* num-col 3) " "))
		    (gen-list (+ (* num-col 3) 1)  (gnc:color-option->hex-string (get-op "Color" "Column Color 1")))
		    (gen-list (+ (* num-col 3) 1) 'left)
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1")))

    ;;Put in the header for all the columns
    (date-bdgt-act-diff-header-table table budget num-col start-period num-periods)

    ;;Put in the income row
    (build-html-row #f
		    #f
		    (gen-list (length income-totals-list) 1)
		    table
		    income-totals-list
		    (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))
			  (three-on-three-off-color num-col))
		    (cons 'left (account-values-align-list (* num-col 3)))
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 2")))

    ;;Put in the expense row
    (build-html-row #f
		    #f
		    (gen-list (length expense-totals-list) 1)
		    table
		    expense-totals-list
		    (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))
			  (three-on-three-off-color num-col))
		    (cons 'left (account-values-align-list (* num-col 3)))
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1")))

    ;;Insert a horizontal line before the totals row
;    (gen-horiz-line (+ (* num-col 3) 1) table)

    ;;Put in the difference row
    (build-html-row #f
		    #t
		    (gen-list (length diff-totals-list) 1)
		    table
		    diff-totals-list
		    (cons (gnc:color-option->hex-string (get-op "Color" "Column Color 1"))
			  (three-on-three-off-color num-col))
		    (cons 'left (account-values-align-list (* num-col 3)))
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 2"))))


  ;; This is the main routine that puts it all together
  (let* ((document (gnc:make-html-document))
	 (table (gnc:make-html-table))
	 (income-totals '())
	 (expense-totals '())
	 (budget (op-value "General" "Budget"))
	 (num-periods (op-value "General" "Number of Report Periods"))
	 (start-period (get-start-period budget (car (gnc:date-option-absolute-time (op-value "General" "Start Date")))))
	 (acnt-list (budget-account-list budget
                                   (gnc-account-get-descendants-sorted (gnc-get-current-root-account))
                                   start-period
                                   num-periods
                                   (op-value "General" "Include accounts with only actual values.") ))
	 (num-col (if (op-value "General" "Generate Year to Date (YTD) column.")
		      (if (op-value "General" "Generate End of Year (EOY) column.")
			  (+ num-periods 2)
			  (+ num-periods 1))
		      (if (op-value "General" "Generate End of Year (EOY) column.")
			  (+ num-periods 1)
			  num-periods))))

    ;;Setup the Title and font color and back ground color of the document
    (gnc:html-document-set-title! document reportname)
    (gnc:html-document-set-style! document "body"
				  'attribute (list "bgcolor" (gnc:color-option->html (get-op "Color" "Background Color")))
				  'font-color (gnc:color-option->html (get-op "Color" "Text Color")))

    (gnc:html-table-set-style! table "table"
			       'attribute (list "border" 0)
			       'attribute (list "cellspacing" 0)
			       'attribute (list "cellpadding" 2))


    ;; Build the Income Table
    (set! income-totals (build-catagory-table table
					      (append (assoc-ref (gnc:decompose-accountlist acnt-list) ACCT-TYPE-INCOME) (assoc-ref (gnc:decompose-accountlist acnt-list) ACCT-TYPE-EQUITY))
					      "Income"
					      budget
					      start-period
					      num-periods
					      num-col))

    ;;Put a blank row inbetween catagories
    (build-html-row #f
		    #f
		    (gen-list (+ (* num-col 3) 1) 1)
		    table
		    (gen-list (+ (* num-col 3) 1) " ")
		    (gen-list (+ (* num-col 3) 1)  (gnc:color-option->hex-string (get-op "Color" "Column Color 1")))
		    (gen-list (+ (* num-col 3) 1) 'left)
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1")))

    ;;Build the Expense Table
    (set! expense-totals (build-catagory-table table
					       (assoc-ref (gnc:decompose-accountlist acnt-list) ACCT-TYPE-EXPENSE)
					       "Expense"
					       budget
					       start-period
					       num-periods
					       num-col))

    ;;Put a blank row inbetween catagories
    (build-html-row #f
		    #f
		    (gen-list (+ (* num-col 3) 1) 1)
		    table
		    (gen-list (+ (* num-col 3) 1) " ")
		    (gen-list (+ (* num-col 3) 1)  (gnc:color-option->hex-string (get-op "Color" "Column Color 1")))
		    (gen-list (+ (* num-col 3) 1) 'left)
		    (gnc:color-option->hex-string (get-op "Color" "Row Color 1")))

    ;;Build and put in the totals table at the end
    (build-totals-table table
			budget
			num-col
			start-period
			num-periods
			(cons "Income Totals" income-totals)
			(cons "Expense Totals" expense-totals)
			(cons "Difference Totals" (diff-income-expense-list income-totals expense-totals 1)))

    ;;Add the generated table to the document
    (gnc:html-document-add-object! document table)

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
