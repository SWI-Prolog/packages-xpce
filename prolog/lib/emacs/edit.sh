#!/bin/sh -f

usage()
{ echo "usage: `basename $0` [+line] file[:line]"
  exit 1
}

line=""

HOST=`hostname`
server="$HOME/.xpce_emacs_server"

case "$DISPLAY" in
  :*)	server=$server.$(echo "$DISPLAY" | sed 's/^:\([0-9]*\).*/\1/')
	;;
  *)	server=$server.$HOST
        ;;
esac

#echo "Server = $server"

if [ ! -S "$server" -o -z "$DISPLAY" ]; then
    server="$HOME/.xpce_emacs_server"
    if [ ! -S "$server" ]; then
	echo "No PceEmacs server"
	exec emacs "$*"
    fi
fi

done=false
while [ $done = false ]; do
    case "$1" in
	+[0-9]*)
		line=`echo "$1" | sed 's/^+//'`
		shift
		;;
	*)
		done=true
		;;
    esac
done

file="$1"

case "$file" in
	"")	usage
		;;
	[~/]*)	;;
	*)	file=`pwd`/$file ;;
esac

case "$file" in
	*:[0-9]*)
		eval `echo $file | sed 's/\(.*\):\([0-9]*\)$/file=\1;line=\2/'`
		;;
esac

if [ "$line" = "" ]; then
	cmd="edit('$file')"
else
	cmd="edit('$file', $line)"
fi

xpce-client $server -bc "$cmd"

if [ $? = 2 ]; then
  if [ "$line" = "" ]; then
    exec emacs "$file";
  else
    exec emacs "$file" +$line
  fi
fi
