#!/bin/sh
#
# gwd:       Starts the genealogy Server
#
# Version:      @(#) /etc/rc.d/init.d/gwd 1.0
#
# description: Starts and stops the genealogy Server at boot time and shutdown.
#
# processname: gwd
# hide: true

# Source function library.
. /etc/rc.d/init.d/functions

# See how we were called.
case "$1" in
  start)
	echo "Starting Geneweb Services:"
	cd /home/geneweb/gw/gw
	/home/geneweb/gw/gw/gwd -daemon
	/home/geneweb/gw/gw/gwsetup -daemon
	touch /var/lock/subsys/gwd
	;;
  stop)
	echo -n "Shutting down Geneweb Services: "
	killproc gwd
	killproc gwsetup
	rm -f /var/lock/subsys/gwd
	echo
	;;
  status)
	status gwd
	;;
  restart)
	echo -n "Restarting Geneweb Services: "
        killproc gwd
        killproc gwsetup
	echo
	cd /home/geneweb/gw/gw
	./gwd -daemon
	./gwsetup -daemon
	touch /var/lock/subsys/gwd
	;;
  *)
	echo "*** Usage: gwd {start|stop|status|restart}"
	exit 1
esac

exit 0
