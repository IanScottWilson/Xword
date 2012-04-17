(ns Xproj.core
(:gen-class) 
(:require [clojure.set :as s]
	  [clojure.contrib.str-utils :as su];maybe lose this and use string
	  [clojure.contrib.math :as math]
	  [clojure.contrib.duck-streams :as ds]
          [clojure.contrib.profile :as p]
          [clojure.string :as cstr]))


;====================== template for html output ===================================
;====================== DO NOT EDIT THIS SECTION BY HAND!===========================
(def html-template "<html>\r\n\r\n<head>\r\n\t<title>PAGE-HEADER</title>\t\r\n</head> \r\n \r\n<body> \r\n\t<!-- \r\n\t\there we have oygContext top DOM element to which the play area will be bound;\r\n\t\tthe HTML template has more binding sites for various visual parts of the puzzle: \r\n\t\toygHeader, oygHeaderMenu, oygState, oygPuzzle, oygPuzzleFooter, oygListH, oygListV, oygFooter;\r\n\t\tall element names for binding are prefixed with \"oyg\"\r\n\t-->\r\n\t<div id=\"oygContext\" align=\"center\" style=\"width:100%;\">\r\n\t\t<table class=\"oyOuterFrame\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">\r\n\t\t\t<tr><td align=\"center\">\r\n\t\t\t\t<table class=\"oyFrame\" border=\"0\" cellpadding=\"0\"cellspacing=\"0\">\r\n\t\t\t\t\t<tr>\r\n\t\t\t\t\t\t<td colspan=\"5\">\r\n\t\t\t\t\t\t\t<table class=\"oyFrame\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">\r\n\t\t\t\t\t\t\t\t<tr class=\"oyHeader\">\r\n\t\t\t\t\t\t\t\t\t<td class=\"oyHeader\">\r\n\t\t\t\t\t\t\t\t\t\t<div id=\"oygHeader\"></div>\r\n\t\t\t\t\t\t\t\t\t</td>\r\n\t\t\t\t\t\t\t\t\t<td align=\"right\">\t\r\n\t\t\t\t\t\t\t\t\t\t<div id=\"oygHeaderMenu\"></div>\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t\t</td> \r\n\t\t\t\t\t\t\t\t</tr>\t\t\t\t\t\r\n\t\t\t\t\t\t\t</table>\r\n\t\t\t\t\t\t</td>  \r\n\t\t\t\t\t</tr>\r\n\t\t\t\t\t<tr style=\"height: 4px;\">\r\n\t\t\t\t\t\t<td colspan=\"5\"></td>\r\n\t\t\t\t\t</tr>\r\n\t\t\t\t\t<tr>  \r\n\t\t\t\t\t\t<td rowspan=\"3\" class=\"oyPuzzleCell\" align=\"center\" valign=\"top\"> \r\n\t\t\t\t\t\t\t<div id=\"oygState\"></div>\r\n\t\t\t\t\t\t\t<div class=\"oyPuzzle\" id=\"oygPuzzle\"></div>\r\n\t\t\t\t\t\t\t<div class=\"oyPuzzleFooter\" id=\"oygPuzzleFooter\"></div>\t\t\t\r\n\t\t\t\t\t\t</td>  \r\n\t\t\t\t\t\t<td class=\"oyListCellDot\">.</td>    \r\n\t\t\t\t\t\t<td class=\"oyListCell\" valign=\"top\" id=\"oygListH\"></td>\r\n\t\t\t\t\t</tr>\r\n\t\t\t\t\t<tr style=\"height: 4px;\">\r\n\t\t\t\t\t\t<td colspan=\"4\"></td>\r\n\t\t\t\t\t</tr>\t\t\r\n\t\t\t\t\t<tr>  \r\n\t\t\t\t\t\t<td class=\"oyListCellDot\">.</td>   \r\n\t\t\t\t\t\t<td class=\"oyListCell\" valign=\"top\" id=\"oygListV\"></td>\t\t\t\t\t\r\n\t\t\t\t\t</tr>\r\n\t\t\t\t\t<tr style=\"height: 4px;\">\r\n\t\t\t\t\t\t<td colspan=\"5\"></td>\r\n\t\t\t\t\t</tr>\t\t\t\r\n\t\t\t\t\t<tr>\r\n\t\t\t\t\t\t<td colspan=\"5\" class=\"oyFooter\"> \r\n\t\t\t\t\t\t\t<div id=\"oygFooter\"></div>\r\n\t\t\t\t\t\t</td>\r\n\t\t\t\t\t</tr>\t\t\t\r\n\t\t\t\t</table>\r\n\t\t\t</td></tr>\r\n\t\t</table>\r\n\t\t<div id=\"oygStatic\" align=\"center\" style=\"font-size: 10px; color: #4282B5; font-family: Arial;\"></div>\r\n\t</div>  \r\n\r\n\t<!-- \r\n\t\there we include all oy-cword-1.0 CSS files; all our style class name are prefixed with \"oy\" \r\n\t-->\r\n\t<link rel=\"stylesheet\" href=\"./oy-cword-1.0/css/base.css\" type=\"text/css\">\r\n\r\n\t<!-- \r\n\t\there we include all oy-cword-1.0 JavaScript files; order is important; \r\n\t\tall the files can be combined into one file to reduce number of separate requests\r\n\t-->\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyPrologue.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyJsrAjax.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyClue.js\"></script>\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyIfan.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyMenu.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyPuzzle.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyServer.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oySign.js\"></script>\t\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyMisc.js\"></script>\t\r\n \r\n\t<script type=\"text/javascript\"><!--\r\n\r\n\t\t// \r\n\t\t//\there we include our own puzzle; it has to be fully prepared with all words properly arranged on the grid;\r\n\t\t//\tcurrently only one instance of the puzzle can be embedded into the page, we may fix this in the future\r\n\t\t// \r\n\r\nvar oygCrosswordPuzzle = new oyCrosswordPuzzle (\r\n\t\t  \"5748185539682739085\",\r\n\t\t  \"./oy-cword-1.0\",\r\n\t\t  \"/a/a\",\r\n\t\t  \"TITLE\",\r\n\t\t  \"SUBTITLE\",\r\n\t\t  PASTEOUTPUTHERE ;\r\n\t\t\r\n\t\t//\r\n\t\t//\there we configure puzzle options, callbacks and publisher information\r\n\t\t// \r\n\t\t \r\n\t\t// publisher information\r\n\t\toygCrosswordPuzzle.publisherName = \"Crossword engine by Pavel Simakov\";\r\n\t\toygCrosswordPuzzle.publisherURL = \"http://www.softwaresecretweapons.com\";\r\n\r\n\t\t// game exit URL\r\n\t\toygCrosswordPuzzle.leaveGameURL = \"http://bigleftshoe.org/ifansblog\";  \r\n      \r\n\t\t// this is how to turn off server support; score submission and action tracking will be disabled\r\n\t\toygCrosswordPuzzle.canTalkToServer = false;\r\n\t\r\n\t--></script>\t\r\n\r\n\t<!--\r\n\t\there we instantiate the puzzle and bind it to the HTML template above and show it to the user\r\n\t-->\r\n\t<script type=\"text/javascript\" src=\"./oy-cword-1.0/js/oyEpilogue.js\"></script>\t\r\n\r\n</body>  \r\n\r\n</html>\r\n")

;=============== defs - globals, string and list utilities ============
(def log-file "xword.log")

(def wordfile "/home/ianto/rhaigeiriau.txt");while testing

(defn strip-dos-yuk
"Removes the rubbish ms text editors add to the end of lines"
  [string]
  (su/re-gsub #"\t" "" string))

(defn get-wordlist
{:doc "Gets user's wordlist. This must be a plain text
file with one, unquoted word per line. Returns a vector
of the words as strings with any upper-case characters 
lower-cased or the symbol :notf if a FileNotFoundException 
is thrown."}
  [file]
  (try
  (vec
   (map #(strip-dos-yuk %) 
	(map #(.toLowerCase %)
	     (map #(su/re-gsub #" " "" %)
		  (ds/read-lines file)))))
  (catch java.io.FileNotFoundException e :notf)))

(def taflen-str-sym
^{:doc "A hash-map strings/keywords of the Welsh alphabet"} 
     (hash-map 
      "a" :a "b" :b "c" :c "ch" :ch "d" :d "dd" :dd "e" :e
      "f" :f "ff" :ff "g" :g "ng" :ng "h" :h "i" :i "j" :j
      "l" :l "ll" :ll "m" :m "n" :n "o" :o "p" :p "ph" :ph
      "r" :r "rh" :rh "s" :s "t" :t "th" :th "u" :u "w" :w "y" :y))

(defn make-eng-tt
{:doc "Returns the English alphabet as a hash-map string/keyword."}
  []
  (zipmap 
   (map str (vec (map char (range 97 123)))) 
   (map keyword(map str (vec (map char (range 97 123)))))))

(def i18n
^{:doc "A map of maps. Keys are the language to be used, vals the language
specific structures required by various functions."}
     {:eng {:transcription-table (make-eng-tt) 
	    :dgrexp #"" 
	    :dgset #{""}}
      :cym {:transcription-table taflen-str-sym 
	    :dgrexp #"ch|dd|ff|ng|ll|ph|rh|th" 
	    :dgset #{"ch" "dd" "ff" "ng" "ll" "ph" "rh" "th"}
	    }})

(def js-tt
{:cym {"ch" "C", "dd" "D", "ff" "F", "ng" "G", "ll" "L", "ph" "P", "rh" "R" "th" "T"}
 :eng {}})

(def normalrexp #"[a-z]")

(defn null-str?
{:doc "Return true if s is empty string"} 
  [s]
  (= s ""))

(defn my-rpn 
{:doc "Splits gair into substrings at boundaries determined by regular expression
re using re-partition from clojure.contrib.str-utils."}
  [re gair]
  (su/re-partition re gair))

(defn my-rpn-seq 
{:doc "Takes a seq thing (list - er not quite, see clojure doc) of strings and partitions 
each according to re unless the string is in the exclusion-set (meant to be used on the result
of parsing a string at digraphs). Should work on anything that can be cast to seq
or automatically gets treated as seq"}
  [str-seq exclusion-set re]
  (map #(if (contains? exclusion-set %) % (my-rpn re %)) str-seq))

(defn total-parse 
{:doc "Will return a seq of strings, parsing gair first by regular expression respecial, then
by recommon. Removes null strings arising from other functions called. Strings in respecialset
are excuded from the second pass parse - meant to correspond to respecial, but could be any
set of strings. Now a multi-arity function. Usually to be called
with a language argument e.g.:cym"}
  ([gair] (total-parse #"" #{""} normalrexp gair))
  ([gair lang] (total-parse ((i18n lang) :dgrexp) ((i18n lang) :dgset) normalrexp gair))
  ([respecial respecialset recommon gair]
  (remove null-str? (-> (my-rpn respecial gair) (my-rpn-seq respecialset recommon) flatten))))

(defn word-to-sym-seq
{:doc "Returns a sequence of symbols representing a word after
parsing according to language lang. Defaults to Welsh if no
language supplied."}
  ([word] (word-to-sym-seq word :cym))
  ([word lang] (for [s (total-parse word lang)] 
		 (((i18n lang) :transcription-table) s))))

(defn sym-seq-to-str
  [v]
  (cstr/join (map name v)))

(defn wordlist-as-symbols
  {:doc "This must be passed lang as a parameter or it defaults
to Welsh spelling."}
  ([wordlist] (wordlist-as-symbols wordlist :cym))
  ([wordlist lang]
     (vec
      (map vec
           (map word-to-sym-seq wordlist
                (repeat (count wordlist) lang))))))

(defn sort-wordlist 
{:doc "Sort the wordlist by length - counting the letters according to
spelling of lang. Defaults to Welsh. I don't reverse it,
though I want longest first, because it will be cast to a vector and the words popped off
- so they come off the end, not the front."}
  ([wordlist] (sort-wordlist wordlist :cym))
([wordlist lang] (sort-by (fn [x] (count (word-to-sym-seq x lang))) wordlist)))

(defn sort-wordlist-sym 
  {:doc "Sort the wordlist by length - this verison takes
a vector of vectors of symbols, so it automatically counts according
to the spelling of the language in use. I don't reverse it,
though I want longest first, because it will be cast to a vector and the words popped off
- so they come off the end, not the front."}
  [wordlist]
  (sort-by (fn [x] (count x)) wordlist))

(defn count-lang
{:doc "Counts length of string according to spelling of lang.
Defaults to Welsh."}
  ([string] (count-lang string :cym))
  ([string lang] (count (word-to-sym-seq string lang))))

(defn dfc
{:doc "dfc stands for 'distance from centre'. Returns how far index is from the centre
of the word as a fraction of the word's length. Sensitive to language - lang -
and defaults to Welsh spelling."}
  ([word index] (dfc word index :cym))
  ([word index lang] 
     (let [centre (cond
		   (odd? (count-lang word lang)) (quot (count-lang word lang) 2)
		   (even? (count-lang word lang)) (/ (dec (count-lang word lang)) 2))]
       (/ (math/abs (- centre index)) (count-lang word lang) ))))


(defn dfc-sym
  {:doc "dfc stands for 'distance from centre'. Returns how far index is from the centre
of the word as a fraction of the word's length.This version assumes a word as a sequence
of symbols (clojure keywords)."}
  [word index]
     (let [centre (cond
		   (odd? (count word)) (quot (count word) 2)
		   (even? (count word)) (/ (dec (count word)) 2))]
       (/ (math/abs (- centre index)) (count word) )))

;==================== functions dealing with grids and their contents=================

(defn occupies 
{:doc "Returns a vector of the consecutive cells a word would occupy if placed.
Word must be passed via word-to-sym-seq to get the cells a word would occupy
according to the spelling rules of the language in use."}
  [word cell direction]
  (let [x (cell 0) y (cell 1)]
    (apply vector
	   (if (= direction :across)
	     (map (fn [_] (vector x (+ y _))) (range (count word))) 
	     (map (fn [_] (vector (+ x _) y)) (range (count word))) ))))

(defn occupies-with-contents-l 
{:doc "Calls occupies then pairs up returned cells with contents as symbols of word,
returning a hash-map with cells as keys, contents as values. Words parsed according
to lang, defaults to :cym"}
[word cell direction]
(zipmap (occupies word cell direction) word))

(defn v-cells-adjacent 
"Returns a sequence - cells immediately above
or below or to the left or right of a sub-row"
[v side]
  (condp = side
     :above (map #(vector (dec (% 0)) (% 1)) v)
     :below (map #(vector (inc (% 0)) (% 1)) v)
     :left  (map #(vector (% 0) (dec (% 1))) v)
     :right (map #(vector (% 0) (inc (% 1))) v)))

(defn v-ends [v direction]
  (condp = direction
     :across (vector (first (v-cells-adjacent v :left )) (last (v-cells-adjacent v :right)))
     :down   (vector (first (v-cells-adjacent v :above)) (last (v-cells-adjacent v :below)))))

(defn exclusion-zone
"Returns forbidden zone for other words of same orientation to occupy. Note this is generic.
To handle Welsh spelling word must be passed via word-to-sym-seq "
 [word cell direction]
 (let [v (occupies word cell direction)]
   (condp = direction
	    :across (-> v 
			(into (v-cells-adjacent v :above)) 
			(into (v-cells-adjacent v :below)) 
			(into (v-ends v :across)))
	    :down   (-> v 
			(into (v-cells-adjacent v :left)) 
			(into (v-cells-adjacent v :right)) 
			(into (v-ends v :down))) )))

(defn not-opposite? [direction1 direction2]
  (= direction1 direction2))

(defn other-direction [direction]
  (condp = direction
	   :across :down
	   :down   :across))

(defn clash-l?
{:doc "Takes two words and their start cells and orientations. Calls occupies-with-contents on
both and looks for same keys - there should only be one key in common at most as it is intended
that this checks across/down intersections. If the contents are the same, return nil, 
otherwise true. Words parsed according to lang which defaults to :cym"}
[word1 st-cell1 direction1 word2 st-cell2 direction2]
(if (not-opposite? direction1 direction2) "Inappropriate call to clash."
    (let [hm1 (occupies-with-contents-l word1 st-cell1 direction1) 
	  hm2 (occupies-with-contents-l word2 st-cell2 direction2)
	  ic  (s/intersection (set (keys hm1)) (set (keys hm2)))] 
    (and (seq ic) (not (= (hm1 (first ic))(hm2 (first ic)) )))))) 
      
;============================= functions dealing with word interactions ======================

(defn cross-set-sym-lang 
{:doc "Expects words as strings which will be parsed according to lang
and checks for common letters. Lang defaults to :cym. Could be passed
word as a coll, in which case no parsing is done (not used like that in this program)"}
  ([gair1 gair2] (cross-set-sym-lang gair1 gair2 :cym))
  ([gair1 gair2 lang]
  (let 
      [gair1s (if (string? gair1) (word-to-sym-seq gair1 lang) gair1)
       gair2s (if (string? gair2) (word-to-sym-seq gair2 lang) gair2)]
  (s/intersection (set gair1s) (set gair2s)))))

(defn can-cross-sym-lang?
{:doc "True if gair1 and gair2 have letters in common in spelling in lang 
- so cath and het don't in Welsh for example and it will return false for 
these words when called with :cym, but true when called with :eng.
Defaults to :cym."}
  ([gair1 gair2] (can-cross-sym-lang? gair1 gair2 :cym))
  ([gair1 gair2 lang]
  (seq (cross-set-sym-lang gair1 gair2 lang))))

(defn indices-of 
  "Returns a vector of integers - the indices of each occurence of item in vector,
Probably should check it is passed a vector; if just a sequence convert??"
  [item vector]
  (loop [i 0 indices [] v  vector ]
  (if (zero? (count v)) 
    indices
   (recur (+ 1 i) (if (= item (first v)) (conj indices i) indices) (vec(rest v))))))

(defn partial-puzzle-sym-lang 
 "Indexes all the positions in word1 where word2 can cross according to spelling of lang.
  Defaults to :cym.
  Returns vectors of pairs of positions: first is word1 position,
  second is word2. This version uses the functions
  that convert letters to symbols as :a etc"
 ([word1 word2] (partial-puzzle-sym-lang word1 word2 :cym))
 ([word1 word2 lang]
  (if (not (can-cross-sym-lang? word1 word2 lang)) 
    nil
    (let [cset (cross-set-sym-lang word1 word2 lang)]
      (loop [cs cset pcs []]
	(if (zero? (count cs))
	  pcs
	  (recur (rest cs) 
		 (into pcs  
			(for [one (indices-of (first cs) (word-to-sym-seq word1 lang)) 
			      two (indices-of (first cs) (word-to-sym-seq word2 lang))] 
			  [word1 one word2 two (first cs)])))))))))

(defn partial-puzzle-sym-f
{:doc "Fast algorithm version. Takes two words and two vectors vectors, words as vectors of symbols. Looks 
for possible crosses using the vectors."}
  ([word1 w-c-1 word2 w-c-2]
     (let [cset (s/intersection (set w-c-1) (set w-c-2))]
     (if (not (seq cset)) nil
	 (loop [cs cset pcs []]
	   (if (zero? (count cs))
	     pcs
	     (recur (rest cs)
		    (into pcs
			  (for [one (indices-of (first cs) w-c-1)
				two (indices-of (first cs) w-c-2)]
			    [word1 one word2 two (first cs)])))))))))
     
(defn heuristic-sort-lang
{:doc "Sorts vectors returned by partial-puzzle-sym-lang according to joint closeness
to centres of both matched words - the smaller the sum of dfc for both of them, the more
'centred' they are jointly."}
  ([v] (heuristic-sort-lang v :cym))
  ([v lang]
     (sort-by #(+ (dfc (nth % 0) (nth % 1) lang) (dfc (nth % 2) (nth % 3) lang)) v)))

(defn heuristic-sym-sort
{:doc "Sorts vectors returned by partial-puzzle-sym-lang according to joint closeness
to centres of both matched words - the smaller the sum of dfc for both of them, the more
'centred' they are jointly."}
  [v]
  (sort-by #(+ (dfc-sym (nth % 0) (nth % 1)) (dfc-sym (nth % 2) (nth % 3))) v))
;=========================== code that guides the fitting of words to a grid ==============

(defn get-new-start-cell
  {:doc "Given a vector returned by partial-puzzle-sym-lang, and data for
a word already in the grid (the word the new word will link in to)
calculates the new (relative) cell 'coordinates'.
link-word-data is [[r c] direction] v is (word1 cp1 word2 cp2 char)"}
  [v link-word-data]
  ;(println "reached get-new-start-cell")
  (condp = (link-word-data 1)
   :across [(- ((link-word-data 0)0) (v 3)) (+ ((link-word-data 0)1) (v 1))]
   :down   [(+ ((link-word-data 0)0) (v 1)) (- ((link-word-data 0)1) (v 3))]))

(defn all-in-direction ;memoize this
{:doc "Returns a grid with all the 'not direction' filtered out."}
 [grid direction]
;maybe can do better with for key and val
 (s/map-invert 
  (select-keys (s/map-invert grid) (filter #(= (nth % 1) direction) (keys (s/map-invert grid))))))

(defn all-cells-in-direction-l ;memoize this
{:doc "Takes the given grid, filters to take all the 'direction' words and
returns a set of all the cells (but without contents)."}
 [grid direction]
 (apply s/union
  (for [item (all-in-direction grid direction)] 
    (set(occupies  (key item) ((val item)0) ((val item) 1))))))

(defn between
  [x endl endr]
  (and (>= x endl) (<= x endr)))

(defn truex-l?
"Checks if word, starting from cell start-cell in direction dir
really will cross the word held in item. Returns nil if not, item if
there is a true crossover.Counts letters according to lang. Default to :cym."
  [item word start-cell dir]
  (let [s1 (set(occupies (item 0) ((item 1)0) ((item 1) 1))) 
	s2 (set(occupies word start-cell dir))]
    (if (empty? (s/intersection s1 s2)) nil item)))

(defn elim-abuts-l
{:doc "returns nil if item does not have a true xword but abuts and forms a possible non-word;
otherwise just returns the item. All letter counting according to lang."}
  [item word start-cell dir]
  (let [s1 (set (exclusion-zone word start-cell dir)) 
	s2 (set (occupies  (item 0) ((item 1)0) ((item 1)1) ))]
  (if  (not (truex-l? item  word start-cell dir));turning to sym seq handled in truex-l?
       (if (seq (s/intersection s1 s2)) nil :red-herring)
       item)))

(defn check-xwords-lang 
{:doc "Tries out word starting in start-cell by applying the neccessary checks - that it
doesn't  inadvertently form new possible 'non-words' and where it crosses another word, 
does so at a matching letter."}
  [grid word start-cell dir]
  (let [d      (if (= dir :across) 1 0)
	endl   (dec (start-cell d)) 
	endr   (+ (start-cell d) (count word))
	xwords (remove nil? (for [item (all-in-direction grid (other-direction dir))] 
				  (if (between (((val item) 0) d) endl endr) item nil)))
	crosscheck (remove #(= % :red-herring) (map #(elim-abuts-l % word start-cell dir) xwords)) ]
    (if (empty? (set crosscheck)) :fits
	(cond 
	 (contains? (set crosscheck) nil) :invalid-new-word
	 (let [cc (map 
		   #(clash-l? 
		     word 
		     start-cell 
		     dir 
		     (nth % 0) 
		     (nth (nth % 1) 0) 
		     (nth (nth % 1) 1) 
		     ) 
		   crosscheck)]
	 (contains? (set cc) true)) :invalid-cross
	 true :fits))))

(defn validates-lang 
{:doc "This is passed an ongoing grid and a proposed cross between a word already in the grid
and a new word, Calculates start cell for new word and
1. checks it wouldn't run alongside already placed words thus forming un-foreseen and therefore
probably nonsense words.
2. checks it doesn't 'graze' rather than actually cross already present words and thus form
nonsense words
3. where it crosses already present words, checks it does so at matching letters. 
If all three tests are passed, the proposed crossing is validated and can go in grid,
otherwise nil is returned to caller (fit)."}
[g v]
;(println g)
;(println v)
(let [old-word (v 0)
	   new-word (v 2)
	   nsc (get-new-start-cell v (g old-word))
	   old-dir ((g old-word) 1)
	   new-dir (other-direction old-dir)
	   ec  (exclusion-zone new-word nsc new-dir)];far too cryptic
    (if (seq (s/intersection (set ec) (all-cells-in-direction-l g new-dir))) nil
    (condp = (check-xwords-lang g new-word nsc new-dir) 
	     :fits                     :validated
	     :invalid-new-word         nil
	     :invalid-cross            nil)))) 

(defn place-in-grid-old
{:doc "Once a new word's position is validated, this slots it in the grid in
the required format (key/val pair) word [[row col] :dir] "}
[grid v]
;(println grid)
;(println v)
(let [link-word-data (grid (nth v 0))]
  (assoc grid 
    (nth v 2) 
    [(get-new-start-cell v link-word-data) (other-direction (nth link-word-data 1))])))

(defmacro my-xor
  [A B]
  `(and (or ~A ~B) (not (and ~A ~B))))

(defn which-if-end?
  [word index]
  (condp = index 
	  0   :first
	  (dec (count word)) :last
	  index false))
	  
   
(defn end-to-body?
  [cross-vector]
  (let [etb1 (which-if-end? (nth cross-vector 0) (nth cross-vector 1))
	   etb2 (which-if-end? (nth cross-vector 2) (nth cross-vector 3))]
      (if (my-xor etb1 etb2)
	(cond
	 (= etb1 :first) {:old :first}
	 (= etb1 :last)  {:old :last}
	 (= etb2 :first) {:new :first}
	 (= etb2 :last)  {:new :last}
	) false)))

(defn get-working-copies
  [grid cross-vector]
  (let [w (end-to-body? cross-vector)
	old-sym-vec ((grid (nth cross-vector 0)) 2)
	old-word (nth cross-vector 0) 
	old-index (nth cross-vector 1) 
	new-word (nth cross-vector 2)
	new-index (nth cross-vector 3)
	new-sym-vec new-word] 
    (cond
     (and (which-if-end? old-word old-index) 
	  (which-if-end? new-word new-index))
     {:old-word old-sym-vec  :new-word new-sym-vec}
     w (condp = w
		{:old :first} {:old-word (assoc old-sym-vec 1 :nil)
			       :new-word new-sym-vec}
		{:old :last}  {:old-word (assoc old-sym-vec (dec(dec (count old-sym-vec))) :nil)
			       :new-word new-sym-vec}
		{:new :first} {:old-word old-sym-vec
			       :new-word (assoc new-sym-vec 1 :nil)}
		{:new :last}  {:old-word old-sym-vec
			       :new-word (assoc new-sym-vec (dec(dec (count new-sym-vec))) :nil)})
     true {:old-word (-> (assoc old-sym-vec old-index :nil)
			 (assoc (dec old-index) :nil)
			 (assoc (inc old-index) :nil))
	   :new-word (-> (assoc new-sym-vec new-index :nil)
			 (assoc (dec new-index) :nil)
			 (assoc (inc new-index) :nil))})))
       
(defn place-in-grid
  [grid v]
  (let [link-word-data 
	 (grid (nth v 0)) 
	 working-copies 
	 (get-working-copies grid v)]
     ;(println "working copies " working-copies)
     ;(println "link-word-data " link-word-data)
    (assoc grid
      (nth v 0) (assoc link-word-data 2 (working-copies :old-word))
      (nth v 2) (vector (get-new-start-cell v link-word-data) 
			(other-direction (nth link-word-data 1))
			(working-copies :new-word)))))
      
    

(defn fit-word
  {:doc "Uses partial-puzzle-sym-lang to find all possible crossings with a word in the grid.
The process is repeated for all words
already in the grid, and then these results are ordered once  using heuristic sort. With this ordered list of possible fits, 'validates'
is repeatedly called until a fit which 'works' is found, or none is found in which case
the word goes to the back of the queue and nil returned. Given a fit, this function returns 
the augmented grid. It could possibly be improved (but definitely made slower) by NOT 
stopping at first fit but finding all and ordering in some other way - e.g., the more words 
the new word will cross, the better - looking for a tighter grid all the time."}
  [grid-list word] 
  (if (zero? (count grid-list)) {word [[0 0] :across word]} ;initialise grid on first call
    (let [poss-fits ;(remove nil? 
			    ;(flatten 
			     (heuristic-sym-sort
			      (vec
			       (apply concat
				      (map 
				       #(partial-puzzle-sym-f 
					 %1 (nth %2 2) word word)
				       (keys grid-list) (vals grid-list))))
				      ;(map #(partial-puzzle-sym-lang  % word lang) 
                                        ;(keys grid-list))))
                              )]
      ;; (println "grid-list is")
      ;; (println grid-list)
      ;; (println "poss-fits is ...")
      ;; (println poss-fits)
      ;; (println "word is ")
      ;; (println word)
      ;; (println "Number of poss-fits")
      ;; (println (count poss-fits))
      (loop [pf poss-fits]
	(cond
	 (zero? (count pf)) nil
	 (validates-lang grid-list (first pf)) (place-in-grid grid-list (first pf))
	 true   (recur (rest pf)))))))

(defn l-identity
[x] x)

(defn l-shuffle
[list] (shuffle list))

(defn prep-list
  ([list f]
     ;; (println "sort function is...")
     ;; (println f)
     (distinct (f list))))

(defn xword-engine
  {:doc "The parameter f, if given explicitly,is meant to be a function that will sort the 
wordlist in some way. As long as the function takes a coll and lang symbol as an argument and returns something 
that can be cast to a vector, any function can be used."}
  ([wordlist grid] (xword-engine wordlist grid sort-wordlist-sym))
  ([wordlist grid f]
     (let [swordlist (vec(prep-list wordlist f))]
       ;; (println "swordlist is ...")
       ;; (println swordlist)
       (loop [wl swordlist g grid f1 (count swordlist) f2 0]
         ;; (println "wordlist now is ...")
         ;; (println wl)
      (let [f3 (if (< f1 f2) f1 100)]
	;; (println "f2= ") (println f2)
        ;; (println "f3= ") (println f3)
      (if (or (zero? (count wl)) (> f2 (* 2 f3)))  g 
	  (let [g1 (fit-word g (peek wl)) 
		wl2 (if (nil? g1) (vec(cons (peek wl) (pop wl))) (vec(pop wl)))
		f11 (count wl2)
		f21 (if (= f11 f1) (inc f2) f2)]
	    (recur wl2 (if (nil? g1) g g1) f11 f21))))))))

;=================== for console display  ===================

(defn max-nsew
  {:doc "Get most negative and positive (N-S) row numbers, likewise columns (E-W)
from grid, return as hash-map {:n int :s int :e int :w int}"}
  [grid]
  {:n (apply min (map #(nth % 0)(map #(nth % 0) (vals grid))))
   :s (apply max (map #(nth % 0)(map #(nth % 0) (vals grid))))
   :e (apply max (map #(nth % 1)(map #(nth % 0) (vals grid))))
   :w (apply min (map #(nth % 1)(map #(nth % 0) (vals grid))))})


(defn new-matrics-nil 
  "This returns a vector of rows (an int) vectors each with
  cols (an int) elements - which can be anything - initialised
  with the value init." 
  [rows cols init]
  (let [r (vec (repeat cols init))]
    (vec (repeat rows r))))

(defn pp-matrics
  "This pretty-prints a matrics, laying out the rows above each other."
  [m]
  (dotimes [row-number (count m)] (println (m row-number))))


(defn overflow?
  [at-n v1 v2]
  (< (count v1) (+ at-n (count v2))))

(defn change-vec-by-vec
{:doc "Overwrites v1 with v2 at index at-n."}
  [at-n v1 v2]
  (cond    
   (overflow? at-n v1 v2) "error: will not fit"
   (zero? at-n) (vec (flatten (vector v2 (subvec v1 (count v2)))))
   (not (zero? at-n)) (vec(flatten (vector (subvec v1 0  at-n) v2 (subvec v1 (+ at-n (count v2))))))))
  
(defn transpose 
  "Expects a vector of vectors (representing an m x n  matrix i.e. 2D array). Does not
  go deeper than the elements of the inner vectors. Passes 
  (transpose (transpose mat))= mat test." 
  [mat]
  (loop [k 0 m []]
    (if (= k  (count (first mat))) 
      m 
     (recur (inc k) (vec (conj m (loop [n 0  v []]
				   (if (= n (count mat)) v
				       (recur (inc n) (vec (conj v (nth (mat n) k))))))))))))


(defn insert-word-in-grid-v
  {:doc "This will assume a word (as vector of items) and grid (matrics type structure) and a 
  grid position  [i j] (row, column) for where the first letter is to go. 
  It will return a grid with the relevant row altered, or fail suitably for no fits. 
  It is indifferent to what the items are."}
  [grid v at-r-c]
  (cond 
   (> (count v) (count (first grid))) 
         (println "Word wider than grid" v (first grid))
   (> (count v) (- (count (first grid)) (at-r-c 1))) 
         (dorun (println "Overflow on right") nil)
   (> 0 (at-r-c 1)) 
         (dorun (println "Overflow on left") nil)
   true 
         (assoc grid (at-r-c 0) (change-vec-by-vec (at-r-c 1) (grid (at-r-c 0)) v))))
  

(defn mpv
{:doc "mpv stands for 'make printable vector' - tokenises word according to langauge alphabet letters taking account of alignment problems."}
([word] (mpv word :cym))  
([word lang]
;uncomment for debugging (println "mpv")(println word)
  (vec (map #(if (= (count %) 1) (str % " ") %) 
	    (map name (word-to-sym-seq word lang))))))

(defn shift-grid
{:doc "Changes (nominal) origin of the grid"}
[grid r-shift c-shift]
(apply hash-map
       (apply concat
	      (for [k (keys grid)] 
		(list k [[(+ r-shift (((grid k)0)0))(+ c-shift (((grid k)0)1))] ((grid k)1)])))))

(defn span-set
  ([word index] (span-set word index :cym))
  ([word index lang]
  (for [x (range (count-lang word lang))] (+ index x))))

(defn total-span-set-dir
     ([grid dir] (total-span-set-dir grid dir :cym))
     ([grid dir lang]
     (let [s (filter  #(= dir (nth % 1)) (vals grid)) g (s/map-invert grid)]
       ;(println s) (println g) for debugging
       (-> (for [item s] 
	     (span-set (g item) 
		       ((item 0) (condp = dir	   
					  :across  1	   
					  :down    0))
		       lang))
	   flatten set))))

(defn width-this-matrics
([grid] (width-this-matrics grid :cym))
([grid lang]
(let [s (total-span-set-dir grid :across lang)]
  (math/abs (inc (- (apply max s) (apply min s)))))))

(defn height-this-matrics
([grid] (height-this-matrics grid :cym))
([grid lang]
(let [s (total-span-set-dir grid :down lang)]
  (math/abs (inc (- (apply max s) (apply min s)))))))

       

(defn invert-v 
[v]
[(v 1) (v 0)])

;This next function is here because it will only be useful for generating
;output for Pavel Simakov's Javascript Crossword Engine (with small adaptations
;by me for welsh di-graphs)

(defn transpose-grid
{:doc "Takes a hash-map representing a crossword - as canonically represented in this
program - and simply interchanges the elements of the start cell for each word
and it's direction to 'other' i.e. :down <--> :across. Useful if an unsymmetric
grid (e.g. is more 'horizontal' than 'vertical') needs to be flipped - consideration
is fitting it on a blog page nicely."}
[grid]
(let [new-vals (for [item (vals grid)] (vector (invert-v (item 0)) (other-direction (item 1))))]
(zipmap (keys grid) new-vals)))

(defn resort-grid-keys
  {:doc "Re-organise a grid, first by sorting by row number, ascending; then by
partitioning the result according to rows, sort each partition by column number.
Recombine into a form suitable for pavelize. This returns the keys (words)
sorted by row and then column."}
  [grid]
  (for [item (apply concat
		    (for [item (partition-by #((% 0)0) (sort(vals grid)))] 
		      (sort-by #((% 0)1) item)))] 
    ((s/map-invert grid) item))) 

(defn js-transcribe
([word] (js-transcribe word :cym))
([word lang] (su/re-gsub ((i18n lang) :dgrexp) #((js-tt lang) %) word)))

(defn pavelize
  {:doc "Takes a canonical crossword grid and produces the Javascript fragment you need to
insert in html template to produce an online crossword using Pavel Simakov's crossword
engine."}
  ([g] (pavelize g :cym))
  ([g lang]
     (let [limits (max-nsew g)
	   row-shift (math/abs (limits :n))
	   col-shift (math/abs (limits :w))
	   grid      (shift-grid g row-shift col-shift)
	   words     (resort-grid-keys grid) 
	   s1 "new oyCrosswordClue( "
	   comma " , "
	   fin "),"
	   finl ")"] 
       (doall (for [w words] 
	               (str 
		       s1 
		       (count-lang w lang) 
		       comma
		       \"" clue" \"
		       comma
		       \" (js-transcribe w lang) \"
		       comma
		       \" (Integer/toHexString (hash w)) \"
		       comma
		       (if (= ((grid w)1) :across) 0 1)
		       comma
		       (((grid w) 0)1)
		       comma
		       (((grid w) 0)0)
		       (if (= w (last words)) finl fin)
		       (char 10)))))))

(defn js-dims
  ([grid] (js-dims grid :cym))
  ([grid lang]
     (str " ," (width-this-matrics grid lang) "," (height-this-matrics grid lang)")")))


 

(defn js-output-np
  ([grid] (js-output-np grid :cym))
  ([grid lang]  
      (.concat
       (.concat
	(.concat "[" (reduce #(.concat %1 %2) "" (pavelize grid lang))) "]")
       (js-dims grid lang))))

(defn js-output
  ([grid] (js-output grid :cym))
  ([grid lang]  
     (print (js-output-np grid lang))))
      

(defn matrics-from-grid
{:doc "Will produce a matrics - vector of vectors - from a completed puzzle (map) with entries in the form 'string [[row column] :direction]'."}
 ([grid] (matrics-from-grid grid :cym))
 ([grid lang]
 (let [limits (max-nsew grid)
       row-shift (math/abs (limits :n))
       col-shift (math/abs (limits :w))
       dim       (max (width-this-matrics grid lang) (height-this-matrics grid lang))
       template  (new-matrics-nil dim dim "..")
       sgrid     (shift-grid grid row-shift col-shift)
       control   (vec (keys sgrid))]
   ;(println sgrid)
   (loop [c control t template]
     (if (zero? (count c)) t
	 (recur (pop c)
		(condp = ((sgrid (peek c)) 1)
			 :across (insert-word-in-grid-v t (mpv (peek c) lang) ((sgrid (peek c)) 0))
			 :down   (transpose 
				  (insert-word-in-grid-v 
				   (transpose t) 
				   (mpv (peek c) lang) 
				   (invert-v ((sgrid (peek c)) 0)))))))))))

(defn display
  ([grid] (display grid :cym))
  ([grid lang]
  (pp-matrics (matrics-from-grid grid lang))))

;==================== user interface from the console =========================

(defn doit [file lang sort-method]
  ;(println "file at doit is ...")
  ;(println file)
  (let [contents (file :contents) name (file :file-name)]
    (println (.concat "Wordlist is ... \n" (str contents)))
    (let [contents-sym (wordlist-as-symbols contents lang)]
      ;(println "contents-sym is ...")
      ;(println contents-sym)
     ;(time (xword-engine contents-sym {} sort-method))))) ;;this line for profiling
      (xword-engine contents-sym {} sort-method))))

(defn user-shift-grid
  [grid]
  (let [limits (max-nsew grid)
	row-shift (math/abs (limits :n))
        col-shift (math/abs (limits :w))]
    (shift-grid grid row-shift col-shift)))

(declare offer-output input-loop2-v)

(defn spit-html
  [grid lang]
  (println "A name for the file to write to? (.html will be added) \n The file will be saved in the current working directory (the one you started the program in) \n unless you provide a full path - just giving a file name is recommended.\n You can move it afterwards.")
  (let [html-file (read-line)]
    (println "The next two options can be left blank if you like - defaults will then be used.")
    (println "A title for your page and puzzle?")
    (let [title (read-line)]
      (println "A subtitle for your puzzle?")
      (let [subtitle (read-line)]
	(let [text
	(su/re-sub #"PAGE-HEADER" title
		   (su/re-sub #"TITLE" title
			      (su/re-sub #"SUBTITLE" subtitle
					 (su/re-sub #"PASTEOUTPUTHERE" (js-output-np grid lang) html-template))))]
	  (spit (.concat html-file ".html") text))))))

(def output-message "Display choices are - \n [g]rid as hash-map,\n [s]hifted grid as hash-map,\n [m]atrix,\n [j]s-output only,\n [p]rint to html")

(defn offer-output-v
  [gmap]
     (loop [message  output-message]
       (println message)
       (let [d (read-line)]
	 (if (contains? #{"g" "s" "m" "j" "p"} d)
	   (let [grid (gmap :thisxword) lang (gmap :thislang)]
	    (condp = d
		     "g" ((println (str grid))(input-loop2-v gmap))
		     "s" ((println (str(user-shift-grid grid)))(input-loop2-v gmap))
		     "m" ((display grid lang) (input-loop2-v gmap))
		     "j" ((js-output grid lang)(input-loop2-v gmap))
		     "p" ((spit-html grid lang)(input-loop2-v gmap))))
	   (recur (.concat (.concat d " is not a valid option \n") output-message))))))

(def language-message "\n ...which alphabet (please enter cym or eng)?")

(def sort-message "\n ...and preferred sort method
                n[one], l[ongest first], s[huffle]  ")

(defn get-word-file []
  (loop [contents :notf message "file name please" file-name :none]
    (if (not (= contents :notf)) {:contents contents :file-name file-name}
    (do (println message)
	(let [file-name (read-line)]
	  (recur (get-wordlist file-name) "file not found \n file name please" file-name))))))

(defn get-language
  []
  (loop [message language-message]
    (println message)
    (let [l (read-line)]
      (if (contains? #{"cym" "eng"} l)
	(keyword l)
	(recur (.concat (.concat l " is not a valid option") language-message))))))

(defn get-sort-method
  []
  (loop [message sort-message]
    (println message)
    (let [s (read-line)]
      (if (contains? #{"n" "l" "s"} s)
	(condp = s
		 "n" l-identity
		 "s" l-shuffle
		 "l" sort-wordlist-sym)
	(recur (.concat (.concat s " is not a valid option") sort-message))))))
	
(defn to-std-form
 [g]
 (zipmap (map sym-seq-to-str (keys g)) (vals g)))
             

(defn input-loop1 []
  (let [word-file (get-word-file)]
    (ds/append-spit log-file (.concat (.concat "Input file " (word-file :file-name)) "\n\n"))
    (println "your file is " (word-file :file-name) )
    (let [lang (get-language)]
      (ds/append-spit log-file (.concat (.concat "Language is " (name lang)) "\n\n")) 
      (println "your language is " lang )
      (let [sm (get-sort-method)]
	(ds/append-spit log-file (.concat (.concat "Sorted by " (.toString sm)) "\n\n"))
	(println (.concat "Working with " (word-file :file-name)))
	(let [thisxword (to-std-form(doit word-file lang sm))]
          (println "thisxword is ...")
          (println thisxword)
	  (dorun (map #(ds/append-spit log-file %)
	       (list
		(.concat (.concat "Result returned at " (. (new java.util.Date) (toString))) "\n\n")
		(.concat (.concat "Returned grid \n\n" (str thisxword)) "\n\n")
		(.concat (.concat "Shifted grid \n\n" (str(user-shift-grid thisxword))) "\n\n")
		(.concat (.concat "Grid as matrix \n\n" (str(matrics-from-grid thisxword lang))) "\n\n")
		(.concat (.concat "Javascript output \n\n" (js-output-np thisxword lang)) "\n\n"))))
	  (input-loop2-v {:thisxword thisxword, :thislang lang})
	  {:thisxword thisxword, :thislang lang})))))

(defn exit []
  (println "Diolch am ddefnyddio'r rhaglen! \n Tan toc!")
  (System/exit 0))


(def next-message "\n you can now: \n 1 Display the grid in some way \n 2 Work with the same grid transposed \n 3 Make another crossword \n 4 Quit \n")

(defn input-loop2-v
  [gmap]
  (loop [message next-message]
   (println message)
   (let [d (read-line)]
     (if (contains? #{"1" "2" "3" "4"} d)
       (condp = d
		"1" (offer-output-v gmap)
		"2" (offer-output-v {:thisxword (transpose-grid(gmap :thisxword)) :thislang (gmap :thislang)})
		"3" (input-loop1)
		"4" (exit))
       (recur (.concat (.concat d " is not a valid option \n") next-message))))))
	 

(def welcome-message "Croeso! \n")

(defn croesair []
  (println welcome-message)
  (ds/spit log-file 
	   (.concat 
	    (.concat 
	     "xword.core session begins at " 
	     (. (new java.util.Date) (toString))) "\n\n"))
  (let [returned-xword (input-loop1)]
    (input-loop2-v returned-xword)))


;============= main for uberjar ==================

(defn -main [& args]
  (croesair))


;============ testing ============================
;; (def file "/home/ianto/tmp/test-project/src/xword/longlist100.txt")

;; (def file2 "/home/ianto/Documents/longlist.txt")

;; (def rawlist (get-wordlist file))

;; (def rawlist2 (get-wordlist file2))

;; (def symlist (wordlist-as-symbols rawlist :eng))

;; (def sorted-symlist (sort-wordlist-sym symlist))

;; (def initial-grid (hash-map (peek (vec sorted-symlist)) [[0, 0] :across (peek (vec sorted-symlist))]))

;; (def g1 {[:x :y :z] [[0 0] :across [:x :y :z]]})


;; ;============ profiling ========================== 

;; (defn my-function [x y]
;;   (let [sum (p/prof :addition (+ x y))
;;         product (p/prof :multiplication (* x y))]
;;     [sum product]))

;; (defmacro do-prof [body]
;;   `(p/profile ~body))




