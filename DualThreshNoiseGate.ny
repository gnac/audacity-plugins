$nyquist plug-in
$version 4
$type process
$name (_"Dual Threshold Noise Gate")
$debugbutton true
$preview enabled
$author (_"David Harty")
$release 0.0.1
$copyright (_"Released under terms of the GNU General Public License version 2 or later")

;; Dual Threshold Noise Gate
;;
;; Like a standard noise gate, but with both an upper AND lower threshold.
;;
;; The gate only CLOSES (applies Level Reduction) when the signal is between
;; the two thresholds -- i.e., in the "noise zone".
;;
;;   Signal ABOVE upper threshold  -> gate open  (voice/signal passes unchanged)
;;   Signal BETWEEN the thresholds -> gate closed (noise is reduced)
;;   Signal BELOW lower threshold  -> gate open  (already at/below noise floor)
;;
;; Typical ACX audiobook usage:
;;   Upper threshold: -25 dB  (just below nominal voice level)
;;   Lower threshold: -60 dB  (the ACX noise-floor limit)
;;   This reduces room tone / breath noise between -25 dB and -60 dB,
;;   while leaving the voice and true silence completely untouched.
;;
;; Attack, Hold, and Decay apply to the upper threshold gate only.
;; The lower threshold opens the gate immediately when the signal
;; drops below it (no hold needed -- it is already quiet enough).


$control MODE "Select Function" choice "Gate,Analyze Noise Level" 0
$control STEREO-LINK "Stereo Linking" choice "Link Stereo Tracks,Don't Link Stereo" 0
$control UPPER-THRESH "Upper gate threshold (dB)" real "" -25 -96 -6
$control LOWER-THRESH "Lower gate threshold (dB)" real "" -60 -96 -6
$control LEVEL-REDUCTION "Level reduction (dB)" real "" -24 -100 0
$control ATTACK "Attack (ms)" real "" 10 1 1000
$control HOLD "Hold (ms)" real "" 50 0 2000
$control DECAY "Decay (ms)" real "" 100 10 4000


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global constants (derived from controls, treated as read-only)

;; SILENCE-FLAG = 1 only when Level Reduction is the lowest possible value
;; (-100 dB), meaning the gate should produce true silence (gain = 0)
;; rather than just a very quiet signal.
(setf SILENCE-FLAG (if (> LEVEL-REDUCTION -96) 0 1))

;; Convert dB values to linear gains / thresholds.
(setf FLOOR     (db-to-linear LEVEL-REDUCTION))  ; gain when gate closed
(setf UPPER-LIN (db-to-linear UPPER-THRESH))
(setf LOWER-LIN (db-to-linear LOWER-THRESH))

;; Convert ms values to seconds.
(setf ATTACK-S   (/ ATTACK 1000.0))
(setf LOOKAHEAD  ATTACK-S)    ; lookahead = attack, same as the standard noise gate
(setf DECAY-S    (/ DECAY  1000.0))
(setf HOLD-S     (/ HOLD   1000.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility

(defun round-up (num)
  (round (+ num 0.5)))


(defun roundn (num places)
  ;; Return NUM rounded to PLACES decimal places as a formatted string.
  ;; Copied from the standard Noise Gate (Steve Daulton).
  (if (= places 0)
      (round num)
      (let* ((x  (format nil "~a" places))
             (ff (strcat "%#1." x "f")))
        (setq *float-format* ff)
        (format nil "~a" num))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error checking

(defun error-check ()
  ;; Need at least 100 samples to do anything meaningful.
  (when (< len 100)
    (throw 'err (format nil
        "Error.~%Insufficient audio selected.~%~
         Make the selection longer than ~a ms."
        (round-up (/ 100000 *sound-srate*)))))
  ;; The lower threshold must be strictly below the upper threshold.
  (when (>= LOWER-THRESH UPPER-THRESH)
    (throw 'err (format nil
        "Error.~%Lower threshold (~a dB) must be below upper threshold (~a dB).~%~
         Adjust the thresholds so that Lower < Upper."
        LOWER-THRESH UPPER-THRESH))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analysis mode
;;
;; To use: select a region of noise-only audio (room tone between
;; words), choose "Analyze Noise Level", and click OK.  The peak
;; of that region is measured and a Lower Threshold is suggested.
;;
;; Upper Threshold cannot be determined from noise-only audio.
;; To find it: select a typical voice section, run
;; Analyze > Measure RMS, then set Upper Threshold a few dB
;; below that measured level.

(defun s-rms (sig &optional (rate 100.0) window-size)
  ;;; Like RMS function but also supports stereo sounds
  ;;; Stereo RMS is the root mean of all (samples ^ 2) [both channels]
  (when (soundp sig)
    (if window-size
        (return-from s-rms (rms sig rate window-size))
        (return-from s-rms (rms sig rate))))
  (let (left-ms right-ms rslt step-size)
    (setf step-size (round (/ (snd-srate (aref sig 0)) rate)))
    (unless window-size
      (setf window-size step-size))
    (setf (aref sig 0) (mult (aref sig 0)(aref sig 0)))
    (setf (aref sig 1) (mult (aref sig 1)(aref sig 1)))
    (setf left-ms (snd-avg (aref sig 0) window-size step-size OP-AVERAGE))
    (setf right-ms (snd-avg (aref sig 1) window-size step-size OP-AVERAGE))
    (s-sqrt (mult 0.5 (sum left-ms right-ms)))))

(defun getfloor ()
  ;; Calculate RMS where rate=10 Hz, window-size=0.4 seconds.
  ;; Return the lowest 0.4 to 0.5 s in the selection.
  (let ((floor 999)
        (window-size (round (* 0.4 *sound-srate*)))
        samples)
    (setf *track* (s-rms *track* 10 window-size))
    ;; Calculate new length in samples without retaining samples in RAM.
    (setf samples (truncate (* len (/ (snd-srate *track*) *sound-srate*))))
    (do ((val (snd-fetch *track*) (snd-fetch *track*))
         (count samples (1- count)))
        ((< count 4) floor) ;stop at last full window.
      (setf floor (min floor val)))))


(defun peak-db (sig test-len)
  ;; Return absolute peak level in dB.
  ;; For stereo, return the louder of the two channels.
  (if (arrayp sig)
      (let ((peakL (peak (aref sig 0) test-len))
            (peakR (peak (aref sig 1) test-len)))
        (linear-to-db (max peakL peakR)))
      (linear-to-db (peak sig test-len))))


(defun analyze (sig)
  ;; Measure peak noise level from the first half-second of the selection.
  ;; Measure noise floor over the entire selection.
  ;; Suggest an upper threshold just above peak,
  ;; and a lower threshold setting just above the noise floor + LEVEL-REDUCTION.
  (let* ((test-length (truncate (min len (/ *sound-srate* 2.0))))
         (peakdb      (peak-db sig test-length))
         (suggested   (+ 1.0 peakdb))
         (floor       (linear-to-db (getfloor))))
    (format nil
      (_ "Noise peak (first ~a s):  ~a dB
Noise floor:  ~a dB
Set upper threshold above peak
Set lower threshold more than \"level reduction\" (~a dB) above the floor.
Suggested Upper Threshold: ~a dB
Suggested Lower Threshold: ~a dB.")
      (roundn (/ test-length *sound-srate*) 2)
      (roundn peakdb 2)
      (roundn floor 2)
      (- LEVEL-REDUCTION)
      (roundn suggested 0)
      (roundn (- floor LEVEL-REDUCTION) 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signal-following helpers
;;
;; These produce the "follower" signals that drive the gate function.
;; For stereo-linked tracks, both channels are merged into a single
;; follower (the louder channel wins).  For unlinked stereo, each
;; channel gets its own follower.

(defun get-follow (sig)
  ;; Return the absolute amplitude of sig, merging channels if linked.
  (let ((follow (multichan-expand #'snd-abs sig)))
    (if (and (arrayp follow) (= STEREO-LINK 0))
        ;; Link Stereo: take the louder channel so both channels gate together.
        (s-max (aref follow 0) (aref follow 1))
        follow)))


(defun get-follow-upper (sig)
  ;; Follower for the upper gate, with Hold-time extension.
  ;; snd-oneshot keeps the follower signal at UPPER-LIN for HOLD-S seconds
  ;; after the signal drops below UPPER-LIN, preventing the gate from
  ;; closing too soon between words / sounds.
  (let ((follow (get-follow sig)))
    (if (> HOLD-S 0)
        (multichan-expand #'snd-oneshot follow UPPER-LIN HOLD-S)
        follow)))


(defun get-follow-lower (sig)
  ;; Follower for the lower gate.  No Hold is applied here: when the signal
  ;; drops below the lower threshold we want the gate to open right away
  ;; because the audio is already quiet enough.
  (get-follow sig))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dual-threshold envelope
;;
;; The envelope is 1.0 (gate open) when the signal is above the upper
;; threshold OR below the lower threshold; it is FLOOR (gate closed)
;; when the signal is between the two thresholds.
;;
;; We build this by combining two standard gate envelopes:
;;
;;   upper-env  = standard gate at UPPER-LIN
;;                1.0 when sig > UPPER-LIN, FLOOR when below
;;
;;   lower-inv  = inverted gate at LOWER-LIN
;;                FLOOR when sig > LOWER-LIN, 1.0 when sig < LOWER-LIN
;;
;; Inversion formula:  lower-inv = FLOOR + (1 - lower-env)
;;   lower-env = 1.0  (above lower thresh)  => inv = FLOOR + 0       = FLOOR
;;   lower-env = FLOOR (below lower thresh) => inv = FLOOR + 1-FLOOR  = 1.0
;;
;; Combined envelope = s-max(upper-env, lower-inv)
;;   sig > UPPER  : upper=1,     inv=FLOOR  -> max = 1     (gate open)
;;   LOWER < sig < UPPER : upper=FLOOR, inv=FLOOR  -> max = FLOOR (gate closed)
;;   sig < LOWER  : upper=FLOOR, inv=1      -> max = 1     (gate open)

(defun get-dual-env (follow-upper follow-lower)
  (let* (
    ;; --- Upper gate ---
    (upper-env (clip (gate follow-upper LOOKAHEAD ATTACK-S DECAY-S FLOOR UPPER-LIN) 1.0))

    ;; --- Lower gate (inverted) ---
    ;; Standard gate at lower threshold: 1.0 above, FLOOR below.
    ;; Lookahead is 0 here (unlike the upper gate).  The upper gate uses lookahead
    ;; so it is already open when voice arrives, preventing clipping.  The lower
    ;; gate has no such concern: we do not want it to start closing the protection
    ;; zone before the signal has actually risen above the lower threshold.
    (lower-env (clip (gate follow-lower 0 ATTACK-S DECAY-S FLOOR LOWER-LIN) 1.0))
    ;; Invert so it is 1.0 BELOW the lower threshold.
    ;; FLOOR + (1 - lower-env) is written as sum(FLOOR, diff(1.0, lower-env)).
    (lower-inv (clip (sum FLOOR (diff 1.0 lower-env)) 1.0))

    ;; --- Combined: gate is open when either condition holds ---
    (dual-env (s-max upper-env lower-inv)))

    ;; Silence-mode correction (matches the original noise gate).
    ;; When SILENCE-FLAG=1, shift the envelope down by FLOOR so the closed
    ;; gate goes to exactly 0 rather than the very-small FLOOR value.
    ;; The matching gain factor in dual-noisegate compensates for open sections.
    (diff dual-env (* SILENCE-FLAG FLOOR))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main gate application

(defun dual-noisegate (sig follow-upper follow-lower)
  ;; Multiply the signal by the dual-threshold gain envelope.
  ;;
  ;; 'gain' is a normalisation scalar that works together with the
  ;; SILENCE-FLAG correction in get-dual-env:
  ;;   Normal mode (SILENCE-FLAG=0): gain=1, env ranges [FLOOR, 1]  -> output [sig*FLOOR, sig]
  ;;   Silence mode (SILENCE-FLAG=1): gain=1/(1-FLOOR), env ranges [0, 1-FLOOR]
  ;;                                                                 -> output [0, sig]
  (let ((gain (/ 1.0 (- 1.0 (* SILENCE-FLAG FLOOR))))
        (env  (get-dual-env follow-upper follow-lower)))
    (mult sig gain env)))


(defun process ()
  (error-check)
  (multichan-expand #'dual-noisegate
                    *track*
                    (get-follow-upper *track*)
                    (get-follow-lower *track*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point

(case MODE
  (0 (catch 'err (process)))
  (t (analyze *track*)))
