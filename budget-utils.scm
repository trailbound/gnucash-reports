(define-module (gnucash report user-reports budget-utils))

(use-modules (gnucash main)) ;; FIXME: dete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash core-utils))

(gnc:module-load "gnucash/report/report-system" 0)


;;(use-modules (ice-9 slib))

;;(gnc:module-load "gnucash/report/report-system" 0)
;;(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url


;; These are some helper functions for looking up option values.
;;(define (get-op section name)
;;  (gnc:lookup-option (gnc:report-options report-obj) section name))
;;
;;(define (op-value section name)
;;  (gnc:option-value (get-op section name)))

;; Accounts --------------------------------------------------------------------

;; Returns a true if account found in budget during the periods
(define (account-in-budget bdgt acnt start-period num-periods)
  (cond
   ((= num-periods 0) #f)
   ((gnc-budget-is-account-period-value-set bdgt acnt start-period) #t)
   (else (account-in-budget bdgt acnt (+ start-period 1) (- num-periods 1)))))


;; Returns a true if account has actual values during the periods
(define (account-has-actual-value bdgt acnt start-period num-periods)
  (cond
   ((= num-periods 0) #f)
   ((not (gnc-numeric-zero-p (gnc-budget-get-account-period-actual-value bdgt acnt start-period))) #t)
   (else (account-has-actual-value bdgt acnt (+ start-period 1) (- num-periods 1)))))


;; Returns a list of accounts for all the periods that have budget items. Setting
;; nonzero-actuals to true will also get all accounts that have non-zero actual
;; value.
(define (budget-account-list bdgt acnt-list start-period num-periods nonzero-actuals)

  ;;If the account list is empty we are done
  (if (null? acnt-list)
      ;; We are done return a blank list
      '()
      ;; Else check to see if the next account is in the budget
      ;; If it is cons the account to with the rest of the accounts
      ;; Else move on to the next account
      (let ((acnt (car acnt-list)))
        (gnc:debug "Acnt Name: " (xaccAccountGetName acnt))
        (gnc:debug "Acnt Placeholder: " (xaccAccountGetPlaceholder acnt))
        (gnc:debug "Acnt Placeholder Descendant: " (xaccAccountGetDescendantPlaceholder acnt))
        (if nonzero-actuals
            ;; nonzero-actuals #t
            (if (or (account-in-budget bdgt acnt start-period num-periods)
                    (account-has-actual-value bdgt acnt start-period num-periods))
                (cons acnt (budget-account-list bdgt (cdr acnt-list) start-period num-periods nonzero-actuals))
                (budget-account-list bdgt (cdr acnt-list) start-period num-periods nonzero-actuals))
            ;; else nonzero-actuals #f
            (if (account-in-budget bdgt acnt start-period num-periods)
                (cons acnt (budget-account-list bdgt (cdr acnt-list) start-period num-periods nonzero-actuals))
                (budget-account-list bdgt (cdr acnt-list) start-period num-periods nonzero-actuals))) )))

(export budget-account-list)

;; Periods ---------------------------------------------------------------------

;; Returns the a commodity collect for the budget, actual, and difference amount
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


;; Returns a list that has a budget values, actual values and the difference for all the periods
;;(define (account-budget-act-diff-row bdgt acnt start-period num-periods)
;;  (let ((comm (xaccAccountGetCommodity acnt))
;;	(bdgt-cc  (gnc:make-commodity-collector))
;;	(act-cc   (gnc:make-commodity-collector))
;;	(diff-cc  (gnc:make-commodity-collector)))
;;
;;    ;;If the number of periods is 0 stop processing
;;    ;; When returning the commodity values just return a value with the commodity symbol in from
;;    (cond
;;     ((= num-periods 0) '())
;;     (else (let ((bdgt-val (gnc-budget-get-account-period-value bdgt acnt start-period))
;;		 (act-val  (gnc-budget-get-account-period-actual-value bdgt acnt start-period)))
;;
;;	     (budget-act-diff-cc bdgt acnt start-period bdgt-cc act-cc diff-cc comm)
;;
;;	     ;;Return the budget actual and differential amount
;;	     (append (list (bdgt-cc 'getmonetary comm #f)
;;			   (act-cc 'getmonetary comm #f)
;;			   (diff-cc 'getmonetary comm #f))
;;		     (account-budget-act-diff-row bdgt acnt (+ start-period 1) (- num-periods 1))))))))

;; Returns the Year to Date budget account and difference value
(define (ytd-budget-act-diff bdgt acnt curr-period end-period bdgt-cc act-cc diff-cc comm)
  (cond
   ((> curr-period end-period) (list (bdgt-cc 'getmonetary comm #f) (act-cc 'getmonetary comm #f) (diff-cc 'getmonetary comm #f)))
   (else (let ()
	   (budget-act-diff-cc bdgt acnt curr-period bdgt-cc act-cc diff-cc comm)
	   (ytd-budget-act-diff bdgt acnt (+ curr-period 1) end-period bdgt-cc act-cc diff-cc comm)))))

