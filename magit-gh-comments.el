;;; magit-gh-comments.el --- Comment on Github Pull Requests via Magit  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2018 Andrew Stahlman

;; Author: Andrew Stahlman <andrewstahlman@gmail.com>
;; Created: 28 Jun 2018
;; Version: 0.1

;; Keywords: tools git vc
;; Homepage: https://github.com/astahlman/magit-gh-comments

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires: ((magit "2.11.0") (magit-gh-pulls "0.5.3") (request "0.3.0") (emacs "26.0"))

;;; Commentary:
;; This package enables the user to view and post comments on Github
;; Pull Requests.

;;; Code:

(require 'subr-x)
(require 'magit-gh-comments-core)
(require 'magit-pull-request)
(require 'magit-review)

;;; magit-gh-comments.el ends here
