(in-package art-social-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter test-artist "14316")

(defun artfacts-artist-search (artist-name)
  "Get the body of the search results page for an artist."
  (get-http-page "http://www.artfacts.net/index.php"
		 (list '("pageType" . "search")
		       (cons "search" artist-name)
		       '("sarea" . "a"))))

(defun artfacts-artist-search-id (body)
  "Parse the body of an artist search results page to get first artist. 
   Results will be id and page, or nil for failure."
  (cl-ppcre:register-groups-bind 
      (id)
      ("href=\"/index.php/pageType/artistInfo/artist/([^/]+)/" body)
    id))

(defun artist-id (artist-name)
  "Find the artist's id and page, or nil if no match."
   (artfacts-artist-search-id (artfacts-artist-search artist-name)))

(defun artfacts-artist-page (artist-id)
  "Get the body of an artist's page."
  (get-http-page 
   (format nil 
	   "http://artfacts.net/index.php/pageType/artistInfo/artist/~a/lang/1"
	   artist-id)
   nil))

(defun artfacts-artist-page-tree (body)
  (chtml:parse body (cxml-stp:make-builder)))

(defun node-is-table-p (node)
  (and (typep node 'stp:element)
       (string= (stp:local-name node) "table")))

(defun most-shown-with-table-p (table)
  "Is the first row a th with a span containing 'Most exhibitions with:'?"
  (when (node-is-table-p table) 
    (let ((tbody (stp:first-child table)))
      (let ((first-row (stp:first-child tbody)))
	(let ((th (stp:first-child first-row)))
	  (when (string= (stp:local-name th) "th")
	    (let ((span (stp:first-child th)))
	      (when (string= (stp:local-name span) "span")
		(string= (stp:data (stp:first-child span)) 
			 "Most exhibitions with:")))))))))

(defstruct artist-info
  artist-name
  artist-id
  most-shown-with)

(defstruct most-shown-with
  artist-name
  rank
  amount
  artist-id)

(defun nth-column-text (n tr)
  "If the nth td of tr just contains text, get it."
  (stp:data (stp:first-child (stp:nth-child n tr))))

(defun artfacts-artists-most-shown-with-table (table)
  (let ((artists '())
	(tbody (stp:first-child table))
	(current-row 0))
    (stp:do-children (row tbody)
      (when (> (incf current-row) 2) ;; Skip first 2 rows - title & headings
	(let* ((td0 (stp:first-child row))
	       (td0-a (stp:first-child td0))
	       (td0-a-href (stp:attribute-value td0-a "href"))
	       (artist-name (stp:data (stp:first-child td0-a))))
	  (cl-ppcre:register-groups-bind (artist-id)
	      ("/artist/([^/]+)/lang/1" td0-a-href)
	    (push (make-most-shown-with 
		   :artist-name (string-trim " " (nbsp-to-space artist-name))
		   :rank (parse-integer (nth-column-text 1 row) )
		   :amount (parse-integer (nth-column-text 2 row))
		   :artist-id (parse-integer artist-id))
		  artists)))))
    artists))

(defun artfacts-find-artists-most-shown-with (parse-tree)
  (stp:do-recursively (node parse-tree)
    (when (most-shown-with-table-p node)
      (return (artfacts-artists-most-shown-with-table node)))))

(defun artfacts-artists-most-shown-with (artist-id)
  (artfacts-find-artists-most-shown-with
   (artfacts-artist-page-tree (artfacts-artist-page artist-id))))




;; Make graph
;; Keep track of depth/extent, don't exceed

(defparameter test-most-shown-with
  (list (MAKE-MOST-SHOWN-WITH
	 :ARTIST-NAME "Francis Alÿs"
	 :RANK 58
	 :AMOUNT 3
	 :ARTIST-ID 9212)
	(MAKE-MOST-SHOWN-WITH
	 :ARTIST-NAME "Aleksandra Mir"
	 :RANK 440
	 :AMOUNT 3
	 :ARTIST-ID 18294)
	(MAKE-MOST-SHOWN-WITH
	 :ARTIST-NAME "Beth Campbell"
	 :RANK 3239
	 :AMOUNT 3
	 :ARTIST-ID 41836)
	(MAKE-MOST-SHOWN-WITH
	 :ARTIST-NAME "Mark Lombardi"
	 :RANK 1618
	 :AMOUNT 4 
	 :ARTIST-ID 18250)))

(defclass most-edge (cl-graph:dot-edge-mixin
			 cl-graph:weighted-edge-mixin
			 cl-graph:graph-container-edge)
  ((most :accessor most)))

(defun vertex-labeller (vertex stream)
  (format stream "~a" (cl-graph:element vertex)))

(defun edge-labeller (edge stream)
  (format stream "~d" (most-shown-with-amount (most edge))))

(defun edge-formatter (edge stream)
  (declare (ignore edge stream)))

(defparameter file-name-base "phelps")

(defun make-graph ()
  (let ((g (cl-graph:make-graph 'cl-graph:graph-container
				:vertex-test #'equal
				:default-edge-class 'most-edge)))
    (loop for artist in test-most-shown-with do
	 (cl-graph:add-edge-between-vertexes 
	  g "Danica Phelps" 
	  (most-shown-with-artist-name artist)
	  :most artist))))

(defun graph-to-svg (g file-name-base) 
  (with-open-file (stream (concatenate 'string file-name-base ".svg")) 
    (cl-graph:graph->dot g nil
			 :vertex-labeler #'vertex-labeller
			 :edge-labeler #'edge-labeller
			 :edge-formatter #'edge-formatter)))

(defun svg-to-dom (file-name-base)
  (cxml:parse-file (concatenate 'string file-name-base ".xml") 
		   (cxml-dom:make-dom-builder)))

;; Search for "edge" class objects && get the value then re-arrange