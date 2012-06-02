<?scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dev-budget.eguile.scm
;; by Greg Brown   gb@trailbound.net
;;
;; This eguile template is designed to be called from
;; dev-budget.scm via the eguile mechanism.
;;
;; $Author: Greg $ $Date: 2012/06/01 21:50:38 $ $Revision: 1.00 $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((version 0.01))
?>

<!-- The HTML starts here... -->
<html>
<head>
<title><?scm:d reportname ?></title>

<?scm (if css? (begin ?>
<link rel="stylesheet" href="<?scm:d opt-css-file ?>" type="text/css">
<!-- Note that the stylesheet file is overridden by some options, i.e.
     opt-font-family and opt-font-size                                 -->
<style type="text/css">
  body {
    <?scm (if opt-font-family (begin ?>
      font-family: <?scm:d opt-font-family ?>;
    <?scm )) ?>
    <?scm (if opt-font-size (begin ?>
      font-size: <?scm:d opt-font-size ?>;
    <?scm )) ?>
  }
  table { /* table does not inherit font sizes for some reason */
    <?scm (if opt-font-size (begin ?>
      font-size: <?scm:d opt-font-size ?>;
    <?scm )) ?>
  }
</style>
<?scm )) ?>

</head>
<body>
<?scm (if (not css?) (begin ?>
  <table border="0" cellpadding="16"><tr><td> <!-- hack for GTKHTML -->
<?scm )) ?>
<h2><?scm:d reportname ?></h2>


<?scm (if (not css?) (begin ?>
  </table> <!-- hack for GTKHTML -->
<?scm )) ?>

</body>
</html>

<?scm
) ; enclosing let
?>