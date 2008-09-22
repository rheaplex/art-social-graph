
;; You may need to install some libraries using asdf-install
;;(require 'asdf)
;;(require 'asdf-install)
;;(asdf-install:install 'drakma)
;;(asdf-install:install 'cl-ppcre)
;;(asdf-install:install 'cl-graph)

(defparameter test-dealer "117511")
(defparameter test-artist "424268441")
(defparameter test-artist-page "danica-phelps.html")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-libraries ()
  (asdf:operate 'asdf:load-op 'drakma)
  (asdf:operate 'asdf:load-op 'cl-ppcre)
  (asdf:operate 'asdf:load-op 'cl-graph))

(defun get-http-page (url parameters)
  "Get the page with the given parameters. Returns mpty string for failure."
  (multiple-value-bind (body result-code)
      (drakma:http-request url
			   :method :get
			   :parameters parameters)
    (if (eq result-code 200)
	body
	"")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artnet: artist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun artnet-get-artist (artist-name)
  "Get the body of the search results page for an artist."
  (get-http-page "http://www.artnet.com/ag/fulltextsearch.asp"
		 (list (cons "searchstring" artist-name)
		       '("currentCategory" . "Artist"))))

(defun artnet-parse-artist (body)
  "Parse the body of an artist search results page to get first artist. 
   Results will be id and page, or nil for failure."
  (cl-ppcre:register-groups-bind 
      (id page) 
      ("href=[\"']/artist/([^/]+)/([^\"']+)[\"']" body)
    (values id page)))


(defun artnet-artist (artist-name)
  "Find the artist's id and page, or nil if no match."
   (artnet-parse-artist (artnet-get-artist artist-name)))

(defun artist-id (artist-name)
  (artnet-artist artist-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artnet: artist's galleries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun artnet-get-dealers-selling (artist-id)
  "Get the body of the search results page for an artist's dealers."
  (get-http-page "http://www.artnet.com/Artists/ArtistHomePage.aspx"
		 (list (cons "artist_id" artist-id)
		       '("page_tab" . "Dealers_selling"))))

(defun artnet-parse-dealers-selling (body)
  "Parse the body of an artist page to get list of dealers, or nil."
  (let ((urls '())) 
    (cl-ppcre:do-register-groups (id page) 
	("href=[\"']/gallery/([^/]+)/([^\"']+)[\"']" body)
      (push (cons id page)
	    urls))
    urls))

(defun dealers-selling (artist-id)
  "Get the list of dealers selling the artist-id, or nil if none."
  (artnet-parse-dealers-selling (artnet-get-dealers-selling artist-id)))

;; TODO: Gallery URL or ID to name
;; Will all galleries have links?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artnet: gallery artists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not all artists have links, though
;; So we need a way of handling artists who just have a name
;; And probably that will be the main way of doing it

(defun artnet-get-dealer-artists (gallery-id)
  "Get the body of the search results page for the list of a gallery's artists."
  (get-http-page "http://www.artnet.com/Galleries/Artists.asp"
		 (list (cons "gid" gallery-id))))

(defun artnet-parse-dealer-artist-ids (body)
  "Parse the body of an dealer artists page to get list of artist ids, or nil."
  (let ((artist-ids '())) 
    ;; Get the full URL
    (cl-ppcre:do-register-groups (url) 
	("href=[\"']Artists_detail.asp\\?([^<]+)" body)
      ;; Extract just the artist ID from the full URL
      (cl-ppcre:register-groups-bind 
	  (id) 
	  ("&aid=([^&]+)" url)
	(push id artist-ids)))
    artist-ids))

(defun gallery-artist-ids (gallery-id)
  (artnet-parse-dealer-artist-ids (artnet-get-dealer-artists gallery-id)))

;; TODO: Parse artist names


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artnet: gallery shows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not all shows will have links, though
;; So we need a way of handling shows that are just date/name
;; And probably that will be the main way of doing it

(defun artnet-get-dealer-shows (dealer-id)
  "Get the body of the page listing a dealer's shows."
  (get-http-page "http://www.artnet.com/Galleries/Exhibitions.asp"
		 (list (cons "gid" dealer-id)
		       '("type" . "2"))))

(defun artnet-parse-dealer-shows (body)
  "Parse the body of an dealer shows page to get list of shows, or nil."
  (let ((show-ids '())) 
    (cl-ppcre:do-register-groups (id) 
      ("href=[\"']Exhibitions.asp\\?gid=[^\&]+&cid=([^&]+)&" body)
      (push id show-ids))
    show-ids))

(defun gallery-shows (dealer-id)
  "Get the list of hyperlineked dealer shows, or nil if none."
  (artnet-parse-dealer-shows (artnet-get-dealer-shows dealer-id)))

;; TODO: parse show names


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artnet: gallery fairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fairs are just name hyphen location, no link

(defun artnet-get-dealer-fairs (dealer-id)
  "Get the body of the page listing a dealer's fairs."
  (get-http-page "http://www.artnet.com/Galleries/ArtFairs.asp"
		 (list (cons "gid" dealer-id)
		       '("type" . "2"))))

(defun artnet-parse-dealer-fairs (body)
  "Parse the body of an dealer fairs page to get list of fairs, or nil."
  (let ((fairs '())) 
    (cl-ppcre:do-register-groups (fair location) 
	;; Name, hyphen, location. Hyphen can be - or \226
      ("<span class=\"darkgreybold\">([\\w\\s]+).([\\w\\s]+)</SPAN>" body)
      ;; Strip leading & trailing whitespace from fair & location
      (push (cons fair location) fairs))
    fairs))

(defun gallery-fairs (dealer-id)
  "Get the list of fair name/location pairs, or nil if none."
  (artnet-parse-dealer-fairs (artnet-get-dealer-fairs dealer-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artnet: artist biography and shows (and art fairs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These two are too difficult to parse for now, so ignore

(defun artnet-get-artist-show-links (artist-id)
  "Get the artist's shows with links. Their bio has unlinked shows."
  (get-http-page "http://www.artnet.com/Artists/ArtistHomePage.aspx"
		 (list (cons "artist_id" artist-id)
		       '("page_tab" . "Exhibitions"))))

(defun artnet-get-artist-fair-links (artist-id)
  "Get the artist's shows with links. Their bio has unlinked shows."
  (get-http-page "http://www.artnet.com/Artists/ArtistHomePage.aspx"
		 (list (cons "artist_id" artist-id)
		       '("page_tab" . "Art_Fairs"))))

;; end ignore

(defun artnet-get-artist-biography (artist-id)
  "Get the artist bio page, used for the bio and for shows without links."
  (get-http-page "http://www.artnet.com/Artists/ArtistHomePage.aspx"
		 (list (cons "artist_id" artist-id)
		       '("page_tab" . "Bio_and_links"))))

(defun artnet-parse-artist-biography-for-biography (body)
  "Parse the artist's bio page for their biography."
  (let ((dates '())
	(events '())) 
    (cl-ppcre:do-register-groups (date) 
      ("<span id=\"Bio_and_links1_repBiography__ctl[^_]+_lblBiographyDate\">([^<]+)</span>" body)
      (push date
	    dates))
    (cl-ppcre:do-register-groups (event) 
      ("<span id=\"Bio_and_links1_repBiography__ctl[^_]+_lblBiographyDescription\">([^>]+)</span>" body)
      (push event
	    events))
    ;; Awards are sometimes dateless biography descriptions, handle this
    (setf events (subseq events (- (length events) (length dates))))
    (map 'list #'cons dates events)))

(defun artnet-parse-artist-biography-for-shows (body)
  "Parse the artist bio page for shows."
  (let ((dates '())
	(shows '())) 
    (cl-ppcre:do-register-groups (date) 
      ("<span id=\"Bio_and_links1_repSelectedExhibitions__ctl[^_]+_lblSelectedExhibitionsDate\">([^<]+)</span>" body)
      (push date
	    dates))
    (cl-ppcre:do-register-groups (show) 
      ("<span id=\"Bio_and_links1_repSelectedExhibitions__ctl[^_]+_lblSelectedExhibitionsDescription\">([^>]+)</span>" body)
      (push show
	    shows))
    (map 'list #'cons dates shows)))

(defun artist-shows (artist-id)
  "Get the artist's selected shows as a list of year/show name pairs."
  (artnet-parse-artist-biography-for-shows 
   (artnet-get-artist-biography artist-id)))

(defun artist-biography (artist-id)
  "Get the artist's biography as a list of year/event name pairs."
  (artnet-parse-artist-biography-for-biography 
   (artnet-get-artist-biography artist-id)))




;; Get artist auction results
;; Need an API key...





;; Make graph
;; Keep track of depth/extent, don't exceed

(let ((g (cl-graph:make-graph 'cl-graph:graph-container)))
            (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
                  (cl-graph:add-edge-between-vertexes g a b))
            (format t "~A" (cl-graph:graph->dot g nil)))
