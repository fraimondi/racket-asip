;; A simple program to display the IP address(es) 
;; It should be invoked by a bash script passing two arguments (eth0 
;; and wlan0 IP addresses, or some other string if not available)
;; This is a possible script:
;; ---- cut here ----
;;#!/bin/bash
;;
;;function get_ip () {
;;  /sbin/ifconfig "$1" | grep "inet addr" | cut -f2 -d: | cut -f1 -d' ' 
;;}
;;
;;function write_ip () {
;;  _IPETH0=$(get_ip eth0)
;;  if [ -z "$_IPETH0" ] ; then
;;    _IPETH0="NONE YET"
;;  fi
;;
;;  _IPWLAN0=$(get_ip wlan0)
;;  if [ -z "$_IPWLAN0" ] ; then
;;    _IPWLAN0="NONE YET"
;;  fi
;;
;;  /opt/racket-6.3/bin/racket /home/pi/franco-tests/racket-asip/write-ip.rkt "$_IPETH0" "$_IPWLAN0"
;;
;;  if [[ "$_IPWLAN0" = "NONE YET" && "$_IPETH0" = "NONE YET" ]]
;;  then
;;    echo "Trying again..."
;;    sleep 2
;;    write_ip
;;  fi
;;}
;;
;;write_ip
;; ---- cut here ----

#lang racket
(require "AsipMain.rkt")
(define args (current-command-line-arguments))
(printf "~a \n" args)
(open-asip)
(sleep 0.5)
(setLCDMessage "  Hi, Mirto is up!" 0)
(setLCDMessage "eth0 IP:" 1)
(setLCDMessage (string-append "  " (vector-ref args 0)) 2)
(setLCDMessage "WiFi IP:" 3)
(setLCDMessage (string-append "  " (vector-ref args 1)) 4)
(close-asip)
