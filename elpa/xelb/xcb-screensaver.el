;;; xcb-screensaver.el --- X11 ScreenSaver extension  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file was generated by 'el_client.el' from 'screensaver.xml',
;; which you can retrieve from <git://anongit.freedesktop.org/xcb/proto>.

;;; Code:

(require 'xcb-types)

(defconst xcb:screensaver:-extension-xname "MIT-SCREEN-SAVER")
(defconst xcb:screensaver:-extension-name "ScreenSaver")
(defconst xcb:screensaver:-major-version 1)
(defconst xcb:screensaver:-minor-version 1)

(require 'xcb-xproto)

(defconst xcb:screensaver:Kind:Blanked 0)
(defconst xcb:screensaver:Kind:Internal 1)
(defconst xcb:screensaver:Kind:External 2)

(defconst xcb:screensaver:Event:NotifyMask 1)
(defconst xcb:screensaver:Event:CycleMask 2)

(defconst xcb:screensaver:State:Off 0)
(defconst xcb:screensaver:State:On 1)
(defconst xcb:screensaver:State:Cycle 2)
(defconst xcb:screensaver:State:Disabled 3)

(defclass xcb:screensaver:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (client-major-version :initarg :client-major-version :type xcb:CARD8)
   (client-minor-version :initarg :client-minor-version :type xcb:CARD8)
   (pad~0 :initform 2 :type xcb:-pad)))
(defclass xcb:screensaver:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (~sequence :type xcb:CARD16)
   (length :type xcb:CARD32)
   (server-major-version :initarg :server-major-version :type xcb:CARD16)
   (server-minor-version :initarg :server-minor-version :type xcb:CARD16)
   (pad~1 :initform 20 :type xcb:-pad)))

(defclass xcb:screensaver:QueryInfo
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)))
(defclass xcb:screensaver:QueryInfo~reply
  (xcb:-reply)
  ((state :initarg :state :type xcb:CARD8)
   (~sequence :type xcb:CARD16)
   (length :type xcb:CARD32)
   (saver-window :initarg :saver-window :type xcb:WINDOW)
   (ms-until-server :initarg :ms-until-server :type xcb:CARD32)
   (ms-since-user-input :initarg :ms-since-user-input :type xcb:CARD32)
   (event-mask :initarg :event-mask :type xcb:CARD32)
   (kind :initarg :kind :type xcb:BYTE)
   (pad~0 :initform 7 :type xcb:-pad)))

(defclass xcb:screensaver:SelectInput
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)
   (event-mask :initarg :event-mask :type xcb:CARD32)))

(defclass xcb:screensaver:SetAttributes
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)
   (x :initarg :x :type xcb:INT16)
   (y :initarg :y :type xcb:INT16)
   (width :initarg :width :type xcb:CARD16)
   (height :initarg :height :type xcb:CARD16)
   (border-width :initarg :border-width :type xcb:CARD16)
   (class :initarg :class :type xcb:BYTE)
   (depth :initarg :depth :type xcb:CARD8)
   (visual :initarg :visual :type xcb:VISUALID)
   (value-mask :initarg :value-mask :type xcb:CARD32)
   (value-list :initform
	       '(expression
		 (xcb:-fieldref 'value-mask)
		 cases
		 ((1 background-pixmap)
		  (2 background-pixel)
		  (4 border-pixmap)
		  (8 border-pixel)
		  (16 bit-gravity)
		  (32 win-gravity)
		  (64 backing-store)
		  (128 backing-planes)
		  (256 backing-pixel)
		  (512 override-redirect)
		  (1024 save-under)
		  (2048 event-mask)
		  (4096 do-not-propogate-mask)
		  (8192 colormap)
		  (16384 cursor)))
	       :type xcb:-switch)
   (background-pixmap :initarg :background-pixmap :type xcb:PIXMAP)
   (background-pixel :initarg :background-pixel :type xcb:CARD32)
   (border-pixmap :initarg :border-pixmap :type xcb:PIXMAP)
   (border-pixel :initarg :border-pixel :type xcb:CARD32)
   (bit-gravity :initarg :bit-gravity :type xcb:CARD32)
   (win-gravity :initarg :win-gravity :type xcb:CARD32)
   (backing-store :initarg :backing-store :type xcb:CARD32)
   (backing-planes :initarg :backing-planes :type xcb:CARD32)
   (backing-pixel :initarg :backing-pixel :type xcb:CARD32)
   (override-redirect :initarg :override-redirect :type xcb:BOOL32)
   (save-under :initarg :save-under :type xcb:BOOL32)
   (event-mask :initarg :event-mask :type xcb:CARD32)
   (do-not-propogate-mask :initarg :do-not-propogate-mask :type xcb:CARD32)
   (colormap :initarg :colormap :type xcb:COLORMAP)
   (cursor :initarg :cursor :type xcb:CURSOR)))

(defclass xcb:screensaver:UnsetAttributes
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)))

(defclass xcb:screensaver:Suspend
  (xcb:-request)
  ((~opcode :initform 5 :type xcb:-u1)
   (suspend :initarg :suspend :type xcb:BOOL)
   (pad~0 :initform 3 :type xcb:-pad)))

(defclass xcb:screensaver:Notify
  (xcb:-event)
  ((~code :initform 0)
   (state :initarg :state :type xcb:BYTE)
   (~sequence :type xcb:CARD16)
   (time :initarg :time :type xcb:TIMESTAMP)
   (root :initarg :root :type xcb:WINDOW)
   (window :initarg :window :type xcb:WINDOW)
   (kind :initarg :kind :type xcb:BYTE)
   (forced :initarg :forced :type xcb:BOOL)
   (pad~0 :initform 14 :type xcb:-pad)))

(defconst xcb:screensaver:event-number-class-alist
  '((0 . xcb:screensaver:Notify))
  "(event-number . event-class) alist.")



(provide 'xcb-screensaver)

;;; xcb-screensaver.el ends here
