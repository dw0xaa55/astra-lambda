(in-package :astra-lambda)

;;;-------------------------------------------------------------------------------------------------
;;; constant variables (metric)
;;;-------------------------------------------------------------------------------------------------
(defparameter +AU+ 149597871000 "one astronomical unit in meters")
(defparameter +LY+ 9460730472580000 "one light year in meters")
(defparameter +PC+ 30856775814913673 "one parsec in meters")
(defparameter +C+  299792458 "the speed of light in meters per second")
(defparameter +G+  6.674e11 "the gravitational constant")

;;;-------------------------------------------------------------------------------------------------
;;; equations
;;;-------------------------------------------------------------------------------------------------
(defun dist-pc-from-parallax-mas (milliarcsec)
  "calculates the distance in parsecs based on the given parallax in milliarcseconds (radians)"
  (/ 1 (/ milliarcsec 1000)))

(defun dist-pc-from-parallax-as (arcsec)
  "calculates the distance in parsecs based on given parallax in arcseconds (radians)"
  (/ 1 arcsec))

(defun emission-max-wavelength (temperature)
  "calculates the wavelength of the emissive maximum of a black body in µm from temperature in Kelvin"
  (/ 2897.8 temperature))

(defun magnitude-from-flux (magnitude-control flux-1 flux-2)
  "calculates the magnitude of a star from its relative flux to a control star with known magnitude"
  (* magnitude-control (* -2.5 (log (/ flux-1 flux-2) 10))))

(defun color-index (magnitude-blue magnitude-red)
  "calculates the color index of a star from blue and red channel instrument; higher means red"
  (* -2.5 (log (/ magnitude-blue magnitude-red) 10))) ;; maybe add the wega-constant to the main formula

;;;-------------------------------------------------------------------------------------------------
;;; conversions
;;;-------------------------------------------------------------------------------------------------
(defun parsec-to-lightyears (parsecs)
  "converts a distance from parsecs to light years"
  (* parsecs 3.26))

(defun degrees-to-radians (degree)
  "converts degrees to radians"
  (/ (* degree PI) 180))

(defun radians-to-degrees (radians)
  "converts radians to degrees"
  (/ (* radians 180) PI))

(defun kelvin-to-celsius (temperature)
  "converts kelvin to celsius"
  (- temperature 273.15))

(defun celsius-to-kelvin (temperature)
  "converts celsius to kelvin"
  (+ temperature 273.15))

(defun right-ascension-to-decimal-degrees (hours minutes seconds)
  "converts right ascension into decimal degrees. seconds may be used with decimal point"
  (+ (* hours 15) (* minutes (/ 1 4)) (* seconds (/ 1 240))))

(defun declination-to-decimal-degrees (degree minutes seconds)
  "converts declination into decimal degrees. seconds may be used with decimal point"
  (+ degree (/ minutes 60) (/ seconds 3600)))

(defun convert-list-data-to-numbers (input-list)
  "converts a list of catalog data from strings to numeric values"
  (mapcar (lambda (x)
	    (if (equal x "")
		(setq x 0.0)
		(setq x (with-input-from-string (in x)
			  (read in)))))
	  input-list))

;;;-------------------------------------------------------------------------------------------------
;;; foreign function interface
;;;-------------------------------------------------------------------------------------------------
(asdf:load-system :cffi)
(cffi:define-foreign-library libcatload
  (:unix "~/.quicklisp/local-projects/astra-lambda/src/libcatload/libcatload.so")
  (t (:default "~/.quicklisp/local-projects/astra-lambda/src/libcatload/" "libcatload.so")))
(cffi:use-foreign-library libcatload)
(cffi:defcfun ("downloadCatalogFromURL" download-catalog-ffi) :void (url :string) (filename :string))

;;;-------------------------------------------------------------------------------------------------
;;; catalog management
;;;-------------------------------------------------------------------------------------------------

;; Example inputs for the function: download-catalog
;; 
;; tap-url         :: https://gea.esac.esa.int/tap-server/tap/
;; catalog-release :: gaiadr2
;; rectacension    ::  245.9
;; declination     :: -26.5
;; radius          ::  0.15
;; format          ::  csv
;; query           :: SELECT * FROM gaiadr2.gaia_source WHERE 1 = CONTAINS(POINT('ICRS',ra,dec),CIRCLE('ICRS',245.89675,-26.52575,0.5)) AND phot_g_mean_mag < 22.0
(defun download-catalog (tap-url catalog-release rectacension declination radius export-format output-filename)
  "fetches catalog data from given parameters and saves it to a specified file"
  (let ((download-link (format nil "~async?REQUEST=doQuery&LANG=ADQL&FORMAT=~a&QUERY=SELECT+*+FROM+~a.gaia_source+WHERE+1+=+CONTAINS(POINT('ICRS',ra,dec),CIRCLE('ICRS',~a,~a,~a))+AND+phot_g_mean_mag+<+22.0" 
			       tap-url export-format catalog-release rectacension declination radius)))
    (format t "Downloading Catalog ... ")
    (download-catalog-ffi download-link output-filename))
  (format t "[DONE]~%"))

(defun load-catalog-from-file (filename)
  "loads catalog data from an aladin export file with csv or tsv extension and returns a list of the catalog data"
  (let ((catalog '()))
    (cond ((not (eq (cl-ppcre:scan ".tsv" filename) nil))
	   (with-open-file (stream filename)
	     (do ((line (read-line stream) (read-line stream nil 'eof)))
		 ((eq line 'eof) filename)
	       (setq catalog (push (cl-ppcre:split "\\s+" line) catalog)))))
	  ((not (eq (cl-ppcre:scan ".csv" filename) nil))
	   (with-open-file (stream filename)
	     (do ((line (read-line stream) (read-line stream nil 'eof)))
		 ((eq line 'eof) filename)
	       (setq catalog (push (cl-ppcre:split "," line) catalog)))))
	  (t (format t "ERROR:unsupported file extension, please use *.csv or *.tsv")))
    (setq catalog (push (car (reverse catalog)) (cddr (reverse catalog))))
    catalog))

(defun get-catalog-architecture (catalog)
  "returns number of columns and rows as well as data field titles"
    (list :columns (length (car catalog)) :rows (- (length catalog) 1) :fields (car catalog)))

(defun get-catalog-data-from-field (catalog field-name &optional selection)
  "returns a list of data from the specified catalog and corresponding data field in strings. optionally a selection can be specified to narrow down the output."
  (let* ((catalog-architecture (get-catalog-architecture catalog))
	 (search-field-index   (position field-name (getf catalog-architecture :fields) :test #'string=))
	 (selection-nil        '())
	 (selection-t          '()))
    (setq selection-nil (mapcar (lambda (x) (nth search-field-index x)) (cdr catalog)))
    (unless (equal selection nil)
      (loop :for i :from 0 :to (length selection) :do
	(when (equal (nth i selection) t)
	  (push (nth i selection-nil) selection-t))))
    (if (equal selection nil)
	(reverse selection-nil)
	(reverse selection-t))))
  
(defun isolate-data (x-values y-values &key x-minimum x-maximum y-minimum y-maximum)
  "isolates the area specified by the minima and maxima for their respective axis and returns a list each for x and y values"
  (let ((new-x-values '())
	(new-y-values '()))
    (mapcar (lambda (x y)
	      (when (and (>= x x-minimum)
			 (<= x x-maximum)
			 (>= y y-minimum)
			 (<= y y-maximum))
		(push x new-x-values)
		(push y new-y-values)))
	    x-values y-values)
    (values (reverse new-x-values) (reverse new-y-values))))

(defun select-data (x-values y-values &key x-minimum x-maximum y-minimum y-maximum)
  "creates a selection list of the specified area to be used with a catalog"
  (let ((selection '()))
    (mapcar (lambda (x y)
	      (if (and (>= x x-minimum)
		       (<= x x-maximum)
		       (>= y y-minimum)
		       (<= y y-maximum))
		  (push t selection)
		  (push nil selection)))
	    x-values y-values)
    selection))

;;;-------------------------------------------------------------------------------------------------
;;; orbital mechanics 
;;;-------------------------------------------------------------------------------------------------
(defun 2d-vector-magnitude (input-vector)
  "returns the magnitude of a vector represented by a list"
  (sqrt (+ (* (first input-vector) (first input-vector))
	   (* (second input-vector) (second input-vector)))))
  
(defun orbit-µ (parent-body-mass)
  "returns the standard gravitational parameter from the parent body mass"
  (* +G+ parent-body-mass))

(defun orbit-h (position-vector velocity-vector)
  "returns the specific angular momentum from the 2-dimensional position- and velocity vectors represented by lists"
  (- (* (first position-vector) (second velocity-vector))
     (* (second position-vector) (first velocity-vector))))

(defun orbit-p (specific-angular-momentum standard-gravitational-parameter)
  "returns the semi latus rectum"
  (/ (* specific-angular-momentum specific-angular-momentum)
     standard-gravitational-parameter))

(defun orbit-epsilon (velocity-magnitude distance-to-planet standard-gravitational-parameter)
  "returns the specific orbital energy"
  (- (/ (* velocity-magnitude velocity-magnitude) 2)
     (/ standard-gravitational-parameter distance-to-planet)))

(defun orbit-hyperbolic-a (standard-gravitational-parameter specific-orbital-energy)
  "returns the semi major axis in a hyperbolic orbit"
  (* -1 (/ standard-gravitational-parameter
	   (* 2 specific-orbital-energy))))

(defun orbit-a (apoapsis-distance periapsis-distance)
  "returns the semi major axis in a eliptical orbit"
  (/ (+ apoapsis-distance periapsis-distance)
     2))

(defun orbit-e-vector (position-vector velocity-vector standard-gravitational-parameter)
  "returns a list with the 2-dimensional eccentricity vector from 2-dimensional position- and velocity vectors represented by lists"
  (let* ((eccentricity-x (/ (- (* (first position-vector)
				  (- (* (2d-vector-magnitude velocity-vector) (2d-vector-magnitude velocity-vector))
				     (/ standard-gravitational-parameter (2d-vector-magnitude position-vector))))
			       (* (first velocity-vector)
				  (+ (* (first position-vector) (first velocity-vector))
				     (* (second position-vector) (second velocity-vector)))))
			    standard-gravitational-parameter))
	 (eccentricity-y (/ (- (* (second position-vector)
				  (- (* (2d-vector-magnitude velocity-vector) (2d-vector-magnitude velocity-vector))
				     (/ standard-gravitational-parameter (2d-vector-magnitude position-vector))))
			       (* (second velocity-vector)
				  (+ (* (first position-vector) (first velocity-vector))
				     (* (second position-vector) (second velocity-vector)))))
			    standard-gravitational-parameter)))
    (list eccentricity-x eccentricity-y)))

(defun orbit-e-from-vector (eccentricity-vector)
  "returns the eccentricity from a known 2-dimensional eccentricity vector represented by a list"
  (sqrt (+ (* (first eccentricity-vector) (first eccentricity-vector))
	   (* (second eccentricity-vector) (second eccentricity-vector)))))

(defun orbit-omega (eccentricity-vector)
  "returns the argument of periapsis from a 2-dimensional eccentricity vector represented by a list"
  (atan (second eccentricity-vector) (first eccentricity-vector)))

(defun orbit-v-radial (velocity-vector position-vector)
  "returns the radial velocity from the 2-dimensional velocity- and position vectors"
  (/ (+ (* (first velocity-vector) (first position-vector))
	(* (second velocity-vector) (second position-vector)))
     (2d-vector-magnitude position-vector)))

(defun orbit-true-anomaly (semi-latus-rectum position-vector eccentricity radial-velocity)
  "returns the true anomaly. the 2-dimensional position vector is represented by a list"
  (let ((f0 (acos (/ (- (/ semi-latus-rectum (2d-vector-magnitude position-vector)) 1) eccentricity))))
    (if (< radial-velocity 0)
	(* f0 -1)
	f0)))

(defun orbit-hyperbolic-anomaly (eccentricity true-anomaly)
  "returns the hyperbolic anomaly"
  (let ((F0 (acosh (/ (+ eccentricity (cos true-anomaly))
		      (+ 1 (* eccentricity (cos true-anomaly)))))))
    (if (< true-anomaly 0)
	(* F0 -1)
	F0)))

(defun orbit-hyperbolic-ttrp (semi-major-axis standard-gravitational-parameter eccentricity hyperbolic-anomaly)
  "returns the time to periapsis in seconds"
  (* (sqrt (/ (* (abs semi-major-axis) (abs semi-major-axis) (abs semi-major-axis))
	      standard-gravitational-parameter))
     (- (* eccentricity (sinh hyperbolic-anomaly))
	hyperbolic-anomaly)))

(defun orbit-r-periapsis (semi-latus-rectum eccentricity)
  "returns the height of the periapsis in m from the gravity source. Subtract the planet's radius for the height from the surface"
  (/ semi-latus-rectum (+ 1 eccentricity)))

(defun orbit-v-periapsis (standard-gravitational-parameter height-of-periapsis semi-major-axis)
  "returns the velocity at periapsis in m/s"
  (sqrt (* standard-gravitational-parameter
	   (- (/ 2 height-of-periapsis)
	      (/ 1 semi-major-axis)))))

(defun orbit-v-ciruclar (standard-gravitational-parameter height-of-target-orbit)
  "returns the velocity of a circular orbit in m/s with an eccentricity of 0.0"
  (sqrt (/ standard-gravitational-parameter height-of-target-orbit)))
