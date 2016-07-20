#!/bin/bash

# from https://github.com/boredomwontgetus/pa-vol
#
# CHANGES, RE-VIEWS/WRITES, IMPROVEMENTS:
# mhasko: functions; VOL-steps;
# gashapon: autodectect default sink
# konrad: more reliable way to autodetect default sink

# get default sink name
SINK_NAME=$(pacmd dump | perl -a -n -e 'print $F[1] if /set-default-sink/')
# try this line instead of the one above if you got problems with the detection of your default sink.
#SINK_NAME=$(pactl stat | grep "alsa_output" | perl -a -n -e 'print $F[2]')

# set max allowed volume; 0x10000 = 100%
VOL_MAX="0x10000"

STEPS="16" # 2^n
VOL_STEP=$((VOL_MAX / STEPS))

VOL_NOW=`pacmd dump | grep -P "^set-sink-volume $SINK_NAME\s+" | perl -p -n -e 's/.+\s(.x.+)$/$1/'`
MUTE_STATE=`pacmd dump | grep -P "^set-sink-mute $SINK_NAME\s+" | perl -p -n -e 's/.+\s(yes|no)$/$1/'`

function plus() {
	VOL_NEW=$((VOL_NOW + VOL_STEP))
	if [ $VOL_NEW -gt $((VOL_MAX)) ]; then
		VOL_NEW=$((VOL_MAX))
	fi
	pactl set-sink-volume $SINK_NAME `printf "0x%X" $VOL_NEW`
	VOL_NOW=$VOL_NEW
}

function minus() {
	VOL_NEW=$((VOL_NOW - VOL_STEP))
	if [ $(($VOL_NEW)) -lt $((0x00000)) ]; then
		VOL_NEW=$((0x00000))
	fi
	pactl set-sink-volume $SINK_NAME `printf "0x%X" $VOL_NEW`
	VOL_NOW=$VOL_NEW
}

function mute() {
	if [ $MUTE_STATE = no ]; then
		pactl set-sink-mute $SINK_NAME 1
		MUTE_STATE=yes
	elif [ $MUTE_STATE = yes ]; then
		pactl set-sink-mute $SINK_NAME 0
		MUTE_STATE=no
	fi
}

function get() {
	BAR=""
	if [ $MUTE_STATE = yes ]; then
		BAR="mute"
		ITERATOR=$((STEPS / 2 - 2))
		while [ $ITERATOR -gt 0 ]; do
			BAR=" ${BAR} "
			ITERATOR=$((ITERATOR - 1))
		done
	else
		DENOMINATOR=$((VOL_MAX / STEPS))
		LINES=$((VOL_NOW / DENOMINATOR))
		DOTS=$((STEPS - LINES))
		while [ $LINES -gt 0 ]; do
			BAR="${BAR}█"
			LINES=$((LINES - 1))
		done
		while [ $DOTS -gt 0 ]; do
			BAR="${BAR}░"
			DOTS=$((DOTS - 1))
		done
	fi
	echo "Audio Volume: $BAR"
}

case "$1" in
	plus)  plus;  get ;;
	minus) minus; get ;;
	mute)  mute;  get ;;
	get)   get
esac
