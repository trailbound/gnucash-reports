;; taxinvoice.scm - this is a gnucash report
;; using the eguile-gnc template processor.
;;
;; (c) 2009 Chris Dennis chris@starsoftanalysis.co.uk
;;
;; $Author: chris $ $Date: 2009/07/02 10:16:07 $ $Revision: 1.23 $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA

; put the (define-module... back when installing as a 'proper' report
; as opposed to referring to it from .gnucash/config.user
; (see http://wiki.gnucash.org/wiki/Custom_Reports )
;(define-module (gnucash report taxinvoice))        

(use-modules (gnucash main))
(use-modules (gnucash gnc-module))        
(use-modules (gnucash business-utils))
(use-modules (gnucash report eguile-gnc))

(use-modules (ice-9 regex))  ; for regular expressions
(use-modules (ice-9 slib))   ; for 'vicinity' functions
(use-modules (srfi srfi-13)) ; for extra string functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities -- some of these should be in a separate module

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful routines to use in the template
;; (these should be in a separate module...)

(define (escape-html s1) 
  ;; Convert string s1 to escape HTML special characters < > and &
  ;; i.e. convert them to &lt; &gt; and &amp; respectively.
  ;; Maybe there's a way to do this in one go... (but order is important)
  (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
  (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
  (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))

(define (nl->br str)
  ;; Replace newlines with <br>
  (regexp-substitute/global #f "\n" str 'pre "<br />" 'post))

(define (nbsp str)
  ;; Replace spaces with &nbsp; (non-breaking spaces)
  ;; (yes, I know <nobr> is non-standard, but webkit splits e.g. "-£40.00" between
  ;; the '-' and the '£' without it.)
  (string-append 
    "<nobr>" 
    (regexp-substitute/global #f " " str 'pre "&nbsp;" 'post) 
    "</nobr>"))

;; Convert any x into something printable
(define (dump x) (escape-html (object->string x)))
(define (ddump x) (display (dump x)))

(define (fmtnumber n)
  ;; Format a number (integer or real) into something printable
  (number->string (if (integer? n) 
                    (inexact->exact n) 
                    n)))

(define (fmtnumeric n)
  ;; Format gnc-numeric n with as many decimal places as required
  (fmtnumber (gnc-numeric-to-double n)))

(define (fmtmoney curr amt)
  ;; Format a monetary amount in the given currency
  (nbsp (gnc:monetary->string (gnc:make-gnc-monetary curr amt)))) 

(define (display-comm-coll-total comm-coll negative?)
  ;; Display the total(s) of a commodity collector
  (for-each
    (lambda (pair)
      (display (nbsp (gnc:monetary->string pair))))
    (comm-coll 'format gnc:make-gnc-monetary negative?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report-specific routines

(define (taxrate taxable taxtable curr)
  ;; Display the tax rate applicable to an invoice line.
  ;; This may be e.g. "15%" or "£5.00" or "15% + £5.00" or "n/a"
  ;; depending on how complicated the tax table is.
  ;; (When called from within the eguile template, anything
  ;; (display)ed becomes part of the HTML string.)
  (if (or (not taxable) (eq? taxtable '()))
    (display "&nbsp;")
    (let* ((amttot  (gnc:make-commodity-collector))
           (pctot   (gnc:make-numeric-collector)) 
           (entries (gncTaxTableGetEntries taxtable))
           (amt?    #f)  ; becomes #t if any entries are amounts
           (pc?     #f)) ; becomes #t if any entries are percentages
      (for-each
        (lambda (entry) 
          (let ((tttype (gncTaxTableEntryGetType   entry))
                (ttamt  (gncTaxTableEntryGetAmount entry)))
            (if (equal? tttype GNC-AMT-TYPE-VALUE)
              (begin
                (set! amt? #t)
                (amttot 'add curr ttamt))
              (begin
                (set! pc? #t)
                (pctot 'add ttamt)))))
        entries)
      (if pc? (begin (display (fmtnumeric (pctot 'total #f))) (display "%")))
      (if (and amt? pc?) (display " +&nbsp;"))        ; both - this seems unlikely in practice
      (if amt?
        (display-comm-coll-total amttot #f))
      (if (and (not amt?) (not pc?)) (display (_ "n/a"))))))        ; neither

(define (coy-info slots key)
  ;; Extract a value from the company info key-value pairs
  (kvp-frame-get-slot-path-gslist
    slots 
    (append gnc:*kvp-option-path* (list gnc:*business-label* key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define all the options

; option pages
(define headingpage  (N_ "Headings 1"))
(define headingpage2 (N_ "Headings 2"))
(define notespage    (N_ "Notes"))
(define filespage    (N_ "Files"))
(define generalpage  gnc:pagename-general)
; option names 
(define optname-report-title   (N_ "Report Title"))
(define optname-invoice-number (N_ "Invoice Number"))
(define optname-font-family    (N_ "Font Family"))
(define optname-font-size      (N_ "Font Size"))
(define optname-logofile       (N_ "Logo File Name"))
(define optname-units          (N_ "Units"))
(define optname-qty            (N_ "Qty"))
(define optname-unit-price     (N_ "Unit Price"))
(define optname-disc-rate      (N_ "Discount Rate"))
(define optname-disc-amount    (N_ "Discount Amount"))
(define optname-net-price      (N_ "Net Price"))
(define optname-tax-rate       (N_ "Tax Rate"))
(define optname-tax-amount     (N_ "Tax Amount"))
(define optname-total-price    (N_ "Total Price"))
(define optname-subtotal       (N_ "Sub-total"))
(define optname-amount-due     (N_ "Amount Due"))
(define optname-payment-recd   (N_ "Payment rec'd..."))
(define optname-extra-notes    (N_ "Extra Notes"))

; Choose only customer invoices
; (This doesn't work very nicely -- all invoices and bills
;  are offered for selection, but if a non-customer invoice
;  is selected, the user is dumped back to viewing the
;  previous invoice (or none) with no error message)
(define (customers-only invoice)
  (let* ((owner     (gncInvoiceGetOwner  invoice))
         (endowner  (gncOwnerGetEndOwner owner))
         (ownertype (gncOwnerGetType     endowner)))
    ;(gnc:debug "ownertype is ")(gnc:debug ownertype)
    (if (eqv? ownertype GNC-OWNER-CUSTOMER)
      (list #t invoice)
      (list #f invoice))))

(define (options-generator)
  ;; Options
  (define report-options (gnc:new-options))
  (define (add-option new-option)
    (gnc:register-option report-options new-option))

  (add-option
    (gnc:make-invoice-option ; defined in gnucash/scm/business-options.scm
      generalpage optname-invoice-number 
      "a" "" (lambda () '()) 
      #f))        ;customers-only)) ;-- see above

  (add-option (gnc:make-string-option
                generalpage optname-font-family "b" 
                (N_ "Font definition in CSS font-family format") "sans"))

  (add-option (gnc:make-string-option
                generalpage optname-font-size "c" 
                (N_ "Font size in CSS font-size format") "medium"))

  (add-option (gnc:make-pixmap-option
                generalpage optname-logofile "d" 
                (N_ "Name of a file containing a logo to be used on the report") 
                ""))

  (add-option (gnc:make-string-option
                ; page / name / orderkey / tooltip / default
                headingpage optname-report-title "a" "" (N_ "Invoice")))

  (add-option (gnc:make-string-option
                headingpage optname-units "b" "" (N_ "Units")))

  (add-option (gnc:make-string-option
                headingpage optname-qty "c" "" (N_ "Qty")))

  (add-option (gnc:make-string-option
                headingpage optname-unit-price "d" "" (N_ "Unit Price")))

  (add-option (gnc:make-string-option
                headingpage optname-disc-rate "e" "" (N_ "Discount Rate")))

  (add-option (gnc:make-string-option
                headingpage optname-disc-amount "f" "" (N_ "Discount Amount")))

  (add-option (gnc:make-string-option
                headingpage optname-net-price "g" "" (N_ "Net Price")))

  (add-option (gnc:make-string-option
                headingpage optname-tax-rate "h" "" (N_ "Tax Rate")))

  (add-option (gnc:make-string-option
                headingpage optname-tax-amount "i" "" (N_ "Tax Amount")))

  (add-option (gnc:make-string-option
                headingpage optname-total-price "j" "" (N_ "Total Price")))

  (add-option (gnc:make-string-option
                headingpage2 optname-subtotal "a" "" (N_ "Sub-total")))

  (add-option (gnc:make-string-option
                headingpage2 optname-amount-due "b" "" (N_ "Amount Due")))

  (add-option (gnc:make-string-option
                headingpage2 optname-payment-recd "c" "" 
                (N_ "Payment received, thank you")))

  (add-option (gnc:make-text-option
                notespage optname-extra-notes "a"
                (N_ "Notes added at end of invoice -- may contain HTML markup") 
                (N_ "(Development version -- don't rely on the numbers on this report without double-checking them.<br>Change the 'Extra Notes' option to get rid of this message)")))

  (gnc:options-set-default-section
    report-options generalpage)

  report-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the report

(define (report-renderer report-obj)
  ;; create the report as a chunk of html, and return it
  ;; as an <html-doc>
  (define (get-opt section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  (define (opt-value section name)
    (gnc:option-value (get-opt section name)))
  (let* ((document                  (gnc:make-html-document))
         (opt-invoice               (opt-value generalpage  optname-invoice-number))
         (opt-font-family           (opt-value generalpage  optname-font-family)) 
         (opt-font-size             (opt-value generalpage  optname-font-size)) 
         (opt-logofile              (opt-value generalpage  optname-logofile)) 
         (opt-report-title          (opt-value headingpage  optname-report-title))
         (opt-units-heading         (opt-value headingpage  optname-units))
         (opt-qty-heading           (opt-value headingpage  optname-qty))
         (opt-unit-price-heading    (opt-value headingpage  optname-unit-price))
         (opt-disc-rate-heading     (opt-value headingpage  optname-disc-rate))
         (opt-disc-amount-heading   (opt-value headingpage  optname-disc-amount))
         (opt-net-price-heading     (opt-value headingpage  optname-net-price))
         (opt-tax-rate-heading      (opt-value headingpage  optname-tax-rate))
         (opt-tax-amount-heading    (opt-value headingpage  optname-tax-amount))
         (opt-total-price-heading   (opt-value headingpage  optname-total-price))
         (opt-subtotal-heading      (opt-value headingpage2 optname-subtotal))
         (opt-amount-due-heading    (opt-value headingpage2 optname-amount-due))
         (opt-payment-recd-heading  (opt-value headingpage2 optname-payment-recd))
         (opt-extra-notes           (opt-value notespage    optname-extra-notes)) 
         (html #f))
    (set! html (eguile-file-to-string 
                 (find-template "taxinvoice.eguile.scm")
                 (the-environment)))
    (gnc:debug "taxinvoice.scm - generated html:") (gnc:debug html)
    (gnc:html-document-add-object! 
      document 
      html)
    document))        ; return the document

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the report

(gnc:define-report
  'version 1
  'name (N_ "Tax Invoice (eguile)")
  'report-guid "0769e242be474010b4acf264a5512e6e"
  'menu-name (N_ "Tax Invoice (eguile)")
  'menu-tip (N_ "Display a customer invoice with tax columns (using eguile template)")
  'menu-path (list gnc:menuname-business-reports)
  'options-generator options-generator
  'renderer report-renderer)

