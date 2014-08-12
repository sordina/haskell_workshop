#!/bin/sh

command="$1"
shift

for i in "$@"
do
	echo
	echo
	echo "<div class='chapter'>"
	echo "<div class='content'>"
	echo
	"$command" < "$i" | sed 's/^\\startmode.*//;s/^\\stopmode.*//'
	echo
	echo "</div>"
	echo "</div>"
	echo
	echo
done
