;;; qzuma-twbs.el --- A tool to insert class twitter bootstrap.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Keywords: convenience, qzuma

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inserting class from twitter bootstrap more convenience with a
;; prompt in minibuffer.

;;; Code:

(require 'qzuma-core)

;; roles

(eval-when-compile
  (defvar qz-twbs-roles
    '(("alert" . "role=\"alert\"")
      ("banner" . "role=\"banner\"")
      ("button" . "role=\"button\"")
      ("complementary" . "role=\"complementary\"")
      ("contentinfo" . "role=\"contentinfo\"")
      ("group" . "role=\"group\"")
      ("main" . "role=\"main\"")
      ("navigation" . "role=\"navigation\"")
      ("presentation" . "role=\"presentation\"")
      ("progressbar" . "role=\"progressbar\"")
      ("search" . "role=\"search\"")
      ("separator" . "role=\"separator\"")
      ("tablist" . "role=\"tablist\"")
      ("toolbar" . "role=\"toolbar\""))))

(qz-define-read-prompt
 "qz-twbs" "role" "group" qz-twbs-roles)

;; arias

(eval-when-compile
  (defvar qz-twbs-arias
    '(("controls" . "aria-controls=\"\"")
      ("describedby" . "aria-describedby=\"\"")
      ("expanded" . "aria-expanded=\"\"")
      ("haspopup" . "aria-haspopup=\"\"")
      ("hidden" . "aria-hidden=\"\"")
      ("label" . "aria-label=\"\"")
      ("labelledby" . "aria-labelledby=\"\"")
      ("pressed" . "aria-pressed=\"\"")
      ("valuemax" . "aria-valuemax=\"\"")
      ("valuemin" . "aria-valuemin=\"\"")
      ("valuenow" . "aria-valuenow=\"\""))))

(qz-define-read-prompt
 "qz-twbs" "aria" "hidden" qz-twbs-arias)

;; data

(eval-when-compile
  (defvar qz-twbs-data
    '(("count" . "data-count=\"\"")
      ("dismiss" . "data-dismiss=\"\"")
      ("example-id" . "data-example-id=\"\"")
      ("lang" . "data-lang=\"\"")
      ("show-count" . "data-show-count=\"\"")
      ("toggle" . "data-toggle=\"\"")
      ("via" . "data-via=\"\""))))

(qz-define-read-prompt
 "qz-twbs" "data" "toggle" qz-twbs-data)

;; classes

(eval-when-compile
  (defvar qz-twbs-classes
    '(("active" . "active")
      ("affix" . "affix")
      ("alert" . "alert")
      ("arrow" . "arrow")
      ("badge" . "badge")
      ("bottom" . "bottom")
      ("breadcrumb" . "breadcrumb")
      ("btn" . "btn")
      ("caption" . "caption")
      ("caret" . "caret")
      ("carousel" . "carousel")
      ("checkbox" . "checkbox")
      ("close" . "close")
      ("collapse" . "collapse")
      ("collapsing" . "collapsing")
      ("container" . "container")
      ("css" . "css")
      ("danger" . "danger")
      ("disabled" . "disabled")
      ("divider" . "divider")
      ("dropdown" . "dropdown")
      ("dropup" . "dropup")
      ("empty" . "")
      ("fade" . "fade")
      ("focus" . "focus")
      ("glyphicon" . "glyphicon")
      ("hidden" . "hidden")
      ("hide" . "hide")
      ("in" . "in")
      ("info" . "info")
      ("initialism" . "initialism")
      ("invisible" . "invisible")
      ("item" . "item")
      ("jumbotron" . "jumbotron")
      ("label" . "label")
      ("lead" . "lead")
      ("left" . "left")
      ("map" . "map")
      ("mark" . "mark")
      ("media" . "media")
      ("modal" . "modal")
      ("nav" . "nav")
      ("navbar" . "navbar")
      ("next" . "next")
      ("open" . "open")
      ("pager" . "pager")
      ("pagination" . "pagination")
      ("panel" . "panel")
      ("popover" . "popover")
      ("prev" . "prev")
      ("previous" . "previous")
      ("progress" . "progress")
      ("radio" . "radio")
      ("right" . "right")
      ("row" . "row")
      ("show" . "show")
      ("small" . "small")
      ("success" . "success")
      ("table" . "table")
      ("thumbnail" . "thumbnail")
      ("tooltip" . "tooltip")
      ("top" . "top")
      ("warning" . "warning")
      ("well" . "well"))))

(qz-define-read-prompt
 "qz-twbs" "class" "alert" qz-twbs-classes
 "class=\"%s\"")

;; srs

(eval-when-compile
  (defvar qz-twbs-srs
    '(("only" . "sr-only")
      ("only-focusable" . "sr-only-focusable"))))

(qz-define-read-prompt
 "qz-twbs" "sr" "only" qz-twbs-srs
 "class=\"%s\"")

;; glyphicons

(eval-when-compile
  (defvar qz-twbs-glyphicons
    '(("adjust" . "glyphicon-adjust")
      ("alert" . "glyphicon-alert")
      ("align-center" . "glyphicon-align-center")
      ("align-justify" . "glyphicon-align-justify")
      ("align-left" . "glyphicon-align-left")
      ("align-right" . "glyphicon-align-right")
      ("apple" . "glyphicon-apple")
      ("arrow-down" . "glyphicon-arrow-down")
      ("arrow-left" . "glyphicon-arrow-left")
      ("arrow-right" . "glyphicon-arrow-right")
      ("arrow-up" . "glyphicon-arrow-up")
      ("asterisk" . "glyphicon-asterisk")
      ("baby-formula" . "glyphicon-baby-formula")
      ("backward" . "glyphicon-backward")
      ("ban-circle" . "glyphicon-ban-circle")
      ("barcode" . "glyphicon-barcode")
      ("bed" . "glyphicon-bed")
      ("bell" . "glyphicon-bell")
      ("bishop" . "glyphicon-bishop")
      ("bitcoin" . "glyphicon-bitcoin")
      ("blackboard" . "glyphicon-blackboard")
      ("bold" . "glyphicon-bold")
      ("book" . "glyphicon-book")
      ("bookmark" . "glyphicon-bookmark")
      ("briefcase" . "glyphicon-briefcase")
      ("btc" . "glyphicon-btc")
      ("bullhorn" . "glyphicon-bullhorn")
      ("calendar" . "glyphicon-calendar")
      ("camera" . "glyphicon-camera")
      ("cd" . "glyphicon-cd")
      ("certificate" . "glyphicon-certificate")
      ("check" . "glyphicon-check")
      ("chevron-down" . "glyphicon-chevron-down")
      ("chevron-left" . "glyphicon-chevron-left")
      ("chevron-right" . "glyphicon-chevron-right")
      ("chevron-up" . "glyphicon-chevron-up")
      ("circle-arrow-down" . "glyphicon-circle-arrow-down")
      ("circle-arrow-left" . "glyphicon-circle-arrow-left")
      ("circle-arrow-right" . "glyphicon-circle-arrow-right")
      ("circle-arrow-up" . "glyphicon-circle-arrow-up")
      ("class" . "glyphicon")
      ("cloud" . "glyphicon-cloud")
      ("cloud-download" . "glyphicon-cloud-download")
      ("cloud-upload" . "glyphicon-cloud-upload")
      ("cog" . "glyphicon-cog")
      ("collapse-down" . "glyphicon-collapse-down")
      ("collapse-up" . "glyphicon-collapse-up")
      ("comment" . "glyphicon-comment")
      ("compressed" . "glyphicon-compressed")
      ("console" . "glyphicon-console")
      ("copy" . "glyphicon-copy")
      ("copyright-mark" . "glyphicon-copyright-mark")
      ("credit-card" . "glyphicon-credit-card")
      ("cutlery" . "glyphicon-cutlery")
      ("dashboard" . "glyphicon-dashboard")
      ("download" . "glyphicon-download")
      ("download-alt" . "glyphicon-download-alt")
      ("duplicate" . "glyphicon-duplicate")
      ("earphone" . "glyphicon-earphone")
      ("edit" . "glyphicon-edit")
      ("education" . "glyphicon-education")
      ("eject" . "glyphicon-eject")
      ("envelope" . "glyphicon-envelope")
      ("equalizer" . "glyphicon-equalizer")
      ("erase" . "glyphicon-erase")
      ("eur" . "glyphicon-eur")
      ("euro" . "glyphicon-euro")
      ("exclamation-sign" . "glyphicon-exclamation-sign")
      ("expand" . "glyphicon-expand")
      ("export" . "glyphicon-export")
      ("eye-close" . "glyphicon-eye-close")
      ("eye-open" . "glyphicon-eye-open")
      ("facetime-video" . "glyphicon-facetime-video")
      ("fast-backward" . "glyphicon-fast-backward")
      ("fast-forward" . "glyphicon-fast-forward")
      ("file" . "glyphicon-file")
      ("film" . "glyphicon-film")
      ("filter" . "glyphicon-filter")
      ("fire" . "glyphicon-fire")
      ("flag" . "glyphicon-flag")
      ("flash" . "glyphicon-flash")
      ("floppy-disk" . "glyphicon-floppy-disk")
      ("floppy-open" . "glyphicon-floppy-open")
      ("floppy-remove" . "glyphicon-floppy-remove")
      ("floppy-save" . "glyphicon-floppy-save")
      ("floppy-saved" . "glyphicon-floppy-saved")
      ("folder-close" . "glyphicon-folder-close")
      ("folder-open" . "glyphicon-folder-open")
      ("font" . "glyphicon-font")
      ("forward" . "glyphicon-forward")
      ("fullscreen" . "glyphicon-fullscreen")
      ("gbp" . "glyphicon-gbp")
      ("gift" . "glyphicon-gift")
      ("glass" . "glyphicon-glass")
      ("globe" . "glyphicon-globe")
      ("grain" . "glyphicon-grain")
      ("hand-down" . "glyphicon-hand-down")
      ("hand-left" . "glyphicon-hand-left")
      ("hand-right" . "glyphicon-hand-right")
      ("hand-up" . "glyphicon-hand-up")
      ("hd-video" . "glyphicon-hd-video")
      ("hdd" . "glyphicon-hdd")
      ("header" . "glyphicon-header")
      ("headphones" . "glyphicon-headphones")
      ("heart" . "glyphicon-heart")
      ("heart-empty" . "glyphicon-heart-empty")
      ("home" . "glyphicon-home")
      ("hourglass" . "glyphicon-hourglass")
      ("ice-lolly" . "glyphicon-ice-lolly")
      ("ice-lolly-tasted" . "glyphicon-ice-lolly-tasted")
      ("import" . "glyphicon-import")
      ("inbox" . "glyphicon-inbox")
      ("indent-left" . "glyphicon-indent-left")
      ("indent-right" . "glyphicon-indent-right")
      ("info-sign" . "glyphicon-info-sign")
      ("italic" . "glyphicon-italic")
      ("jpy" . "glyphicon-jpy")
      ("king" . "glyphicon-king")
      ("knight" . "glyphicon-knight")
      ("lamp" . "glyphicon-lamp")
      ("leaf" . "glyphicon-leaf")
      ("level-up" . "glyphicon-level-up")
      ("link" . "glyphicon-link")
      ("list" . "glyphicon-list")
      ("list-alt" . "glyphicon-list-alt")
      ("lock" . "glyphicon-lock")
      ("log-in" . "glyphicon-log-in")
      ("log-out" . "glyphicon-log-out")
      ("magnet" . "glyphicon-magnet")
      ("map-marker" . "glyphicon-map-marker")
      ("menu-down" . "glyphicon-menu-down")
      ("menu-hamburger" . "glyphicon-menu-hamburger")
      ("menu-left" . "glyphicon-menu-left")
      ("menu-right" . "glyphicon-menu-right")
      ("menu-up" . "glyphicon-menu-up")
      ("minus" . "glyphicon-minus")
      ("minus-sign" . "glyphicon-minus-sign")
      ("modal-window" . "glyphicon-modal-window")
      ("move" . "glyphicon-move")
      ("music" . "glyphicon-music")
      ("new-window" . "glyphicon-new-window")
      ("object-align-bottom" . "glyphicon-object-align-bottom")
      ("object-align-horizontal" . "glyphicon-object-align-horizontal")
      ("object-align-left" . "glyphicon-object-align-left")
      ("object-align-right" . "glyphicon-object-align-right")
      ("object-align-top" . "glyphicon-object-align-top")
      ("object-align-vertical" . "glyphicon-object-align-vertical")
      ("off" . "glyphicon-off")
      ("oil" . "glyphicon-oil")
      ("ok" . "glyphicon-ok")
      ("ok-circle" . "glyphicon-ok-circle")
      ("ok-sign" . "glyphicon-ok-sign")
      ("open" . "glyphicon-open")
      ("open-file" . "glyphicon-open-file")
      ("option-horizontal" . "glyphicon-option-horizontal")
      ("option-vertical" . "glyphicon-option-vertical")
      ("paperclip" . "glyphicon-paperclip")
      ("paste" . "glyphicon-paste")
      ("pause" . "glyphicon-pause")
      ("pawn" . "glyphicon-pawn")
      ("pencil" . "glyphicon-pencil")
      ("phone" . "glyphicon-phone")
      ("phone-alt" . "glyphicon-phone-alt")
      ("picture" . "glyphicon-picture")
      ("piggy-bank" . "glyphicon-piggy-bank")
      ("plane" . "glyphicon-plane")
      ("play" . "glyphicon-play")
      ("play-circle" . "glyphicon-play-circle")
      ("plus" . "glyphicon-plus")
      ("plus-sign" . "glyphicon-plus-sign")
      ("print" . "glyphicon-print")
      ("pushpin" . "glyphicon-pushpin")
      ("qrcode" . "glyphicon-qrcode")
      ("queen" . "glyphicon-queen")
      ("question-sign" . "glyphicon-question-sign")
      ("random" . "glyphicon-random")
      ("record" . "glyphicon-record")
      ("refresh" . "glyphicon-refresh")
      ("registration-mark" . "glyphicon-registration-mark")
      ("remove" . "glyphicon-remove")
      ("remove-circle" . "glyphicon-remove-circle")
      ("remove-sign" . "glyphicon-remove-sign")
      ("repeat" . "glyphicon-repeat")
      ("resize-full" . "glyphicon-resize-full")
      ("resize-horizontal" . "glyphicon-resize-horizontal")
      ("resize-small" . "glyphicon-resize-small")
      ("resize-vertical" . "glyphicon-resize-vertical")
      ("retweet" . "glyphicon-retweet")
      ("road" . "glyphicon-road")
      ("rub" . "glyphicon-rub")
      ("ruble" . "glyphicon-ruble")
      ("save" . "glyphicon-save")
      ("save-file" . "glyphicon-save-file")
      ("saved" . "glyphicon-saved")
      ("scale" . "glyphicon-scale")
      ("scissors" . "glyphicon-scissors")
      ("screenshot" . "glyphicon-screenshot")
      ("sd-video" . "glyphicon-sd-video")
      ("search" . "glyphicon-search")
      ("send" . "glyphicon-send")
      ("share" . "glyphicon-share")
      ("share-alt" . "glyphicon-share-alt")
      ("shopping-cart" . "glyphicon-shopping-cart")
      ("signal" . "glyphicon-signal")
      ("sort" . "glyphicon-sort")
      ("sort-by-alphabet" . "glyphicon-sort-by-alphabet")
      ("sort-by-alphabet-alt" . "glyphicon-sort-by-alphabet-alt")
      ("sort-by-attributes" . "glyphicon-sort-by-attributes")
      ("sort-by-attributes-alt" . "glyphicon-sort-by-attributes-alt")
      ("sort-by-order" . "glyphicon-sort-by-order")
      ("sort-by-order-alt" . "glyphicon-sort-by-order-alt")
      ("sound-dolby" . "glyphicon-sound-dolby")
      ("sound-stereo" . "glyphicon-sound-stereo")
      ("star" . "glyphicon-star")
      ("star-empty" . "glyphicon-star-empty")
      ("stats" . "glyphicon-stats")
      ("step-backward" . "glyphicon-step-backward")
      ("step-forward" . "glyphicon-step-forward")
      ("stop" . "glyphicon-stop")
      ("subscript" . "glyphicon-subscript")
      ("subtitles" . "glyphicon-subtitles")
      ("sunglasses" . "glyphicon-sunglasses")
      ("superscript" . "glyphicon-superscript")
      ("tag" . "glyphicon-tag")
      ("tags" . "glyphicon-tags")
      ("tasks" . "glyphicon-tasks")
      ("tent" . "glyphicon-tent")
      ("text-background" . "glyphicon-text-background")
      ("text-color" . "glyphicon-text-color")
      ("text-height" . "glyphicon-text-height")
      ("text-size" . "glyphicon-text-size")
      ("text-width" . "glyphicon-text-width")
      ("th" . "glyphicon-th")
      ("th-large" . "glyphicon-th-large")
      ("th-list" . "glyphicon-th-list")
      ("thumbs-down" . "glyphicon-thumbs-down")
      ("thumbs-up" . "glyphicon-thumbs-up")
      ("time" . "glyphicon-time")
      ("tint" . "glyphicon-tint")
      ("tower" . "glyphicon-tower")
      ("transfer" . "glyphicon-transfer")
      ("trash" . "glyphicon-trash")
      ("tree-conifer" . "glyphicon-tree-conifer")
      ("tree-deciduous" . "glyphicon-tree-deciduous")
      ("triangle-bottom" . "glyphicon-triangle-bottom")
      ("triangle-left" . "glyphicon-triangle-left")
      ("triangle-right" . "glyphicon-triangle-right")
      ("triangle-top" . "glyphicon-triangle-top")
      ("unchecked" . "glyphicon-unchecked")
      ("upload" . "glyphicon-upload")
      ("usd" . "glyphicon-usd")
      ("user" . "glyphicon-user")
      ("volume-down" . "glyphicon-volume-down")
      ("volume-off" . "glyphicon-volume-off")
      ("volume-up" . "glyphicon-volume-up")
      ("warning-sign" . "glyphicon-warning-sign")
      ("wrench" . "glyphicon-wrench")
      ("xbt" . "glyphicon-xbt")
      ("yen" . "glyphicon-yen")
      ("zoom-in" . "glyphicon-zoom-in")
      ("zoom-out" . "glyphicon-zoom-out"))))

(qz-define-read-prompt
 "qz-twbs" "glyphicon" "class" qz-twbs-glyphicons
 "<span class=\"glyphicon %s\" aria-hidden=\"true\"></span> ")

;; dropdowns

(eval-when-compile
  (defvar qz-twbs-dropdowns
    '(("backdrop" . "dropdown-backdrop")
      ("class" . "dropdown")
      ("header" . "dropdown-header")
      ("menu" . "dropdown-menu")
      ("menu-left" . "dropdown-menu-left")
      ("menu-right" . "dropdown-menu-right")
      ("toggle" . "dropdown-toggle"))))

(qz-define-read-prompt
 "qz-twbs" "dropdown" "class" qz-twbs-dropdowns
 "class=\"dropdown %s\"")

;; btns

(eval-when-compile
  (defvar qz-twbs-btns
    '(("block" . "btn-block")
      ("class" . "btn")
      ("danger" . "btn-danger")
      ("default" . "btn-default")
      ("group" . "btn-group")
      ("group-justified" . "btn-group-justified")
      ("group-lg" . "btn-group-lg")
      ("group-sm" . "btn-group-sm")
      ("group-vertical" . "btn-group-vertical")
      ("group-xs" . "btn-group-xs")
      ("info" . "btn-info")
      ("lg" . "btn-lg")
      ("link" . "btn-link")
      ("primary" . "btn-primary")
      ("sm" . "btn-sm")
      ("success" . "btn-success")
      ("toolbar" . "btn-toolbar")
      ("warning" . "btn-warning")
      ("xs" . "btn-xs"))))

(qz-define-read-prompt
 "qz-twbs" "btn" "class" qz-twbs-btns
 "class=\"btn %s\"")

;; inputs

(eval-when-compile
  (defvar qz-twbs-inputs
    '(("class" . "input")
      ("group" . "input-group")
      ("group-addon" . "input-group-addon")
      ("group-btn" . "input-group-btn")
      ("group-lg" . "input-group-lg")
      ("group-sm" . "input-group-sm")
      ("lg" . "input-lg")
      ("sm" . "input-sm"))))

(qz-define-read-prompt
 "qz-twbs" "input" "class" qz-twbs-inputs
 "class=\"input %s\"")

;; cols

(eval-when-compile
  (qz-define-list-range
   "qz-twbs-cols" 12
   ("lg" . "col-lg")
   ("lg-offset" . "col-lg-offset")
   ("lg-pull" . "col-lg-pull")
   ("lg-push" . "col-lg-push")
   ("md" . "col-md")
   ("md-offset" . "col-md-offset")
   ("md-pull" . "col-md-pull")
   ("md-push" . "col-md-push")
   ("sm" . "col-sm")
   ("sm-offset" . "col-sm-offset")
   ("sm-pull" . "col-sm-pull")
   ("sm-push" . "col-sm-push")
   ("xs" . "col-xs")
   ("xs-offset" . "col-xs-offset")
   ("xs-pull" . "col-xs-pull")
   ("xs-push" . "col-xs-push")))

(qz-define-read-prompt
 "qz-twbs" "col" "sm-12" qz-twbs-cols
 "class=\"%s\"")

;; navs

(eval-when-compile
  (defvar qz-twbs-navs
    '(("class" . "nav")
      ("divider" . "nav-divider")
      ("justified" . "nav-justified")
      ("pills" . "nav-pills")
      ("stacked" . "nav-stacked")
      ("tabs" . "nav-tabs")
      ("tabs-justified" . "nav-tabs-justified"))))

(qz-define-read-prompt
 "qz-twbs" "nav" "class" qz-twbs-navs
 "class=\"nav %s\"")

;; navbars

(eval-when-compile
  (defvar qz-twbs-navbars
    '(("brand" . "navbar-brand")
      ("btn" . "navbar-btn")
      ("class" . "navbar")
      ("collapse" . "navbar-collapse")
      ("default" . "navbar-default")
      ("fixed-bottom" . "navbar-fixed-bottom")
      ("fixed-top" . "navbar-fixed-top")
      ("form" . "navbar-form")
      ("header" . "navbar-header")
      ("inverse" . "navbar-inverse")
      ("left" . "navbar-left")
      ("link" . "navbar-link")
      ("nav" . "navbar-nav")
      ("right" . "navbar-right")
      ("static-top" . "navbar-static-top")
      ("text" . "navbar-text")
      ("toggle" . "navbar-toggle"))))

(qz-define-read-prompt
 "qz-twbs" "navbar" "class" qz-twbs-navbars
 "class=\"navbar %s\"")

;; paginations

(eval-when-compile
  (defvar qz-twbs-paginations
    '(("class" . "pagination")
      ("lg" . "pagination-lg")
      ("sm" . "pagination-sm"))))

(qz-define-read-prompt
 "qz-twbs" "pagination" "class" qz-twbs-paginations
 "class=\"pagination %s\"")

;; labels

(eval-when-compile
  (defvar qz-twbs-labels
    '(("class" . "label")
      ("danger" . "label-danger")
      ("default" . "label-default")
      ("info" . "label-info")
      ("primary" . "label-primary")
      ("success" . "label-success")
      ("warning" . "label-warning"))))

(qz-define-read-prompt
 "qz-twbs" "label" "class" qz-twbs-labels
 "class=\"label %s\"")

;; alerts

(eval-when-compile
  (defvar qz-twbs-alerts
    '(("class" . "alert")
      ("danger" . "alert-danger")
      ("dismissable" . "alert-dismissable")
      ("dismissible" . "alert-dismissible")
      ("info" . "alert-info")
      ("link" . "alert-link")
      ("success" . "alert-success")
      ("warning" . "alert-warning"))))

(qz-define-read-prompt
 "qz-twbs" "alert" "class" qz-twbs-alerts
 "class=\"alert %s\"")

;; progresses

(eval-when-compile
  (defvar qz-twbs-progresses
    '(("bar" . "progress-bar")
      ("bar-danger" . "progress-bar-danger")
      ("bar-info" . "progress-bar-info")
      ("bar-striped" . "progress-bar-striped")
      ("bar-success" . "progress-bar-success")
      ("bar-warning" . "progress-bar-warning")
      ("class" . "progress")
      ("striped" . "progress-striped"))))

(qz-define-read-prompt
 "qz-twbs" "progress" "class" qz-twbs-progresses
 "class=\"%s\"")

;; medias

(eval-when-compile
  (defvar qz-twbs-medias
    '(("body" . "media-body")
      ("bottom" . "media-bottom")
      ("class" . "media")
      ("heading" . "media-heading")
      ("left" . "media-left")
      ("list" . "media-list")
      ("middle" . "media-middle")
      ("object" . "media-object")
      ("right" . "media-right"))))

(qz-define-read-prompt
 "qz-twbs" "media" "class" qz-twbs-medias
 "class=\"%s\"")

;; lists

(eval-when-compile
  (defvar qz-twbs-lists
    '(("group" . "list-group")
      ("group-item" . "list-group-item")
      ("group-item-danger" . "list-group-item-danger")
      ("group-item-heading" . "list-group-item-heading")
      ("group-item-info" . "list-group-item-info")
      ("group-item-success" . "list-group-item-success")
      ("group-item-text" . "list-group-item-text")
      ("group-item-warning" . "list-group-item-warning")
      ("inline" . "list-inline")
      ("unstyled" . "list-unstyled"))))

(qz-define-read-prompt
 "qz-twbs" "list" "group" qz-twbs-lists
 "class=\"%s\"")

;; panels

(eval-when-compile
  (defvar qz-twbs-panels
    '(("body" . "panel-body")
      ("class" . "panel")
      ("collapse" . "panel-collapse")
      ("danger" . "panel-danger")
      ("default" . "panel-default")
      ("footer" . "panel-footer")
      ("group" . "panel-group")
      ("heading" . "panel-heading")
      ("info" . "panel-info")
      ("primary" . "panel-primary")
      ("success" . "panel-success")
      ("title" . "panel-title")
      ("warning" . "panel-warning"))))

(qz-define-read-prompt
 "qz-twbs" "panel" "class" qz-twbs-panels
 "class=\"panel %s\"")

;; tables

(eval-when-compile
  (defvar qz-twbs-tables
    '(("bordered" . "table-bordered")
      ("class" . "table")
      ("condensed" . "table-condensed")
      ("hover" . "table-hover")
      ("responsive" . "table-responsive")
      ("striped" . "table-striped"))))

(qz-define-read-prompt
 "qz-twbs" "table" "class" qz-twbs-tables
 "class=\"table %s\"")

;; embeds

(eval-when-compile
  (defvar qz-twbs-embeds
    '(("responsive" . "embed-responsive")
      ("responsive-16by9" . "embed-responsive-16by9")
      ("responsive-4by3" . "embed-responsive-4by3")
      ("responsive-item" . "embed-responsive-item"))))

(qz-define-read-prompt
 "qz-twbs" "embed" "responsive" qz-twbs-embeds
 "class=\"%s\"")

;; wells

(eval-when-compile
  (defvar qz-twbs-wells
    '(("class" . "well")
      ("lg" . "well-lg")
      ("sm" . "well-sm"))))

(qz-define-read-prompt
 "qz-twbs" "well" "class" qz-twbs-wells
 "class=\"well %s\"")

;; containers

(eval-when-compile
  (defvar qz-twbs-containers
    '(("class" . "container")
      ("fluid" . "container-fluid"))))

(qz-define-read-prompt
 "qz-twbs" "container" "class" qz-twbs-containers
 "class=\"%s\"")

;; modals

(eval-when-compile
  (defvar qz-twbs-modals
    '(("backdrop" . "modal-backdrop")
      ("body" . "modal-body")
      ("class" . "modal")
      ("content" . "modal-content")
      ("dialog" . "modal-dialog")
      ("footer" . "modal-footer")
      ("header" . "modal-header")
      ("lg" . "modal-lg")
      ("open" . "modal-open")
      ("scrollbar-measure" . "modal-scrollbar-measure")
      ("sm" . "modal-sm")
      ("title" . "modal-title"))))

(qz-define-read-prompt
 "qz-twbs" "modal" "class" qz-twbs-modals
 "class=\"%s\"")

;; carousels

(eval-when-compile
  (defvar qz-twbs-carousels
    '(("caption" . "carousel-caption")
      ("class" . "carousel")
      ("control" . "carousel-control")
      ("indicators" . "carousel-indicators")
      ("inner" . "carousel-inner"))))

(qz-define-read-prompt
 "qz-twbs" "carousel" "class" qz-twbs-carousels
 "class=\"%s\"")

;; forms

(eval-when-compile
  (defvar qz-twbs-forms
    '(("control" . "form-control")
      ("control-feedback" . "form-control-feedback")
      ("control-static" . "form-control-static")
      ("group" . "form-group")
      ("group-lg" . "form-group-lg")
      ("group-sm" . "form-group-sm")
      ("horizontal" . "form-horizontal")
      ("inline" . "form-inline"))))

(qz-define-read-prompt
 "qz-twbs" "form" "control" qz-twbs-forms
 "class=\"%s\"")

(provide 'qzuma-twbs)
;;; qzuma-twbs.el ends here
