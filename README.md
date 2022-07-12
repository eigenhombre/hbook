# hbook

![build](https://github.com/eigenhombre/hbook/actions/workflows/build.yml/badge.svg)

<img src="/cosb-crab-88.png" width="500">

*HBOOK histogram from [an analysis of high energy cosmic ray data performed in 1988](https://ui.adsabs.harvard.edu/abs/1992ssbg.book.....B/abstract)*

# hbook

Text-based histograms in Common Lisp inspired by CERN's FORTRAN
histograming library, [HBOOK](https://cds.cern.ch/record/307945/files/).

`hbook` implements a key subset of the functionality of the original
library.  It accepts a list of numbers and generates a text-based
histogram.  Though perhaps crude by today's standards, this approach
(from 1975!) can be helpful for spot-checking bulk behaviors of
measurements or random deviates at the REPL, without having to mess
around with getting a graphical interface working.

# Usage

First, set up [Quicklisp](https://www.quicklisp.org/beta/) and
[Ultralisp](https://ultralisp.org/).  Then,

    (ql:quickload :hbook)

# API

The API currently consists of a single function:

    (hbook <list-of-nums> &optional (nbins 50) (height 5))

The generated output shows the shape of the distribution, the exact
bin heights, and the "x-values" of (the centers of) the bins.

`hbook` returns a string, which can be printed immediately, stored for later, etc.

# Examples

Two six-sided dice:

    ;; Single die:
    (defun d () (1+ (random 6)))
    ;; n dice, summed:
    (defun dn (n) (loop repeat n sum (d)))

    (printc (hbook (loop repeat 100000 collect (dn 2))
                   11
                   20))

gives

     15720      X
     14892      X
     14065      X
     13238     XXX
     12410     XXX
     11583     XXX
     10756    XXXXX
      9928    XXXXX
      9101    XXXXX
      8274   XXXXXXX
      7446   XXXXXXX
      6619   XXXXXXX
      5791   XXXXXXX
      4964  XXXXXXXXX
      4137  XXXXXXXXX
      3309  XXXXXXXXX
      2482 XXXXXXXXXXX
      1655 XXXXXXXXXXX
       827 XXXXXXXXXXX
         0 XXXXXXXXXXX

              11111
           25813641852
           75217502367
           56927425112
           80856756537

                   111
           23456789012
           ...........
           00000000000
           00000000000

Sum of 100 dice rolls (illustrating the [Central Limit Theorem](https://en.wikipedia.org/wiki/Central_limit_theorem)):

    (princ (hbook (loop repeat 30000 collect (dn 3000))
                  50
                  20))

gives

      1729                        XXX
      1638                        XXXXX
      1547                      XXXXXXX
      1456                      XXXXXXXX
      1365                     XXXXXXXXXX
      1274                    XXXXXXXXXXXX
      1183                    XXXXXXXXXXXX
      1092                   XXXXXXXXXXXXX
      1001                   XXXXXXXXXXXXXXX
       910                  XXXXXXXXXXXXXXXX
       819                 XXXXXXXXXXXXXXXXX
       728                 XXXXXXXXXXXXXXXXXX
       637                 XXXXXXXXXXXXXXXXXX
       546               XXXXXXXXXXXXXXXXXXXXX
       455               XXXXXXXXXXXXXXXXXXXXXX
       364              XXXXXXXXXXXXXXXXXXXXXXXX
       273             XXXXXXXXXXXXXXXXXXXXXXXXXX
       182            XXXXXXXXXXXXXXXXXXXXXXXXXXXX
        91          XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         0 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

                             111111111111111
                    11234568913456778765320075543211
              11244816324634932181692182796069121440744211
           15714018621471271267506920509905642087269881075653

           11111111111111111111111111111111111111111111111111
           00000000000000000000000000000000000000000000000000
           11112222222333333344444445555555666666677777778888
           57891245689123568902356790234679013467801345780124
           71593715937159371593715937159371593715937159371594
           ..................................................
           00000111112222233333444445555566666777788888999990
           02468024680257913579135791357913579135802468024680

Just for fun, an exponential deviate:

    (princ (hbook (loop repeat 1000000
                        collect (* 50 (- (log (random 1.0)))))
                  50
                  20))

    192145  X
    182032  X
    171919  X
    161806  X
    151694  XX
    141581  XX
    131468  XX
    121355 XXX
    111242 XXXX
    101129 XXXX
     91016 XXXXX
     80903 XXXXX
     70790 XXXXXX
     60677 XXXXXX
     50564 XXXXXXX
     40452 XXXXXXXX
     30339 XXXXXXXXX
     20226 XXXXXXXXXX
     10113 XXXXXXXXXXXXX
         0 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

           1211
           2051975432111
           2259105124941865432111
           421259085839489100383186433211
           155879141542826328837022890273876433211
           38975374093912745856748487529521272033186656341114

                   111111112222222333333334444444455555556666
            1235679013456890234678012356790134578902346780124
           03692581470369269258147036925825814703692581581470
           ..................................................
           00122344566788900123345567789901123345567889001223
           07407418518529629630730741741852852963963073074184

# Disclaimer

After looking through the ancient manuals from CERN, and finding no
evidence of a trademark or similar encumbrance, I borrowed the name
`hbook` from the original CERN library because this library overlaps
substantially in terms of functionality; but the original HBOOK
library is old enough that any other correspondence or conflict is
unlikely, and no actual code from the original library is used in this
one.

# License

MIT.  See LICENSE file.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
