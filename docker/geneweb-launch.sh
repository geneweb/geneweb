#!/bin/bash
GENEWEB_HOME="/usr/local/share/geneweb"
GENEWEB_BASE_PATH=${GENEWEB_HOME}/share/data
GENEWEB_DB=${GENEWEB_DB:=database}
GENEWEB_DB_PATH=${GENEWEB_BASE_PATH}/${GENEWEB_DB}
GENEWEB_ADMIN=${GENEWEB_ADMIN:=admin}
GENEWEB_ADMIN_PASS=${GENEWEB_ADMIN_PASS:=$(openssl rand -hex 32)}
GENEWEB_LANG=${GENEWEB_LANG:=en}

gwlaunch_log() {
	echo "$(date +%Y-%m-%d_%H:%M:%S) geneweb-launch: $1"
}

set_db_config() {
	SETTING_NAME=${1,,}
	SETTING_VALUE=$2
	gwlaunch_log "-- Setting $SETTING_NAME to $SETTING_VALUE in database ${GENEWEB_DB}."
	if grep -q "^$SETTING_NAME=.*$" ${GENEWEB_DB_PATH}.gwf; then
		sed -i "s/^$SETTING_NAME=.*$/$SETTING_NAME=$SETTING_VALUE/" ${GENEWEB_DB_PATH}.gwf
	else
		echo "$SETTING_NAME=$SETTING_VALUE" >> ${GENEWEB_DB_PATH}.gwf
	fi
}

init() {
	# Create default database if it doesn't exist
	if [ ! -f ${GENEWEB_DB_PATH}.gwf ] || [ ! -d ${GENEWEB_DB_PATH}.gwb ]; then
		gwlaunch_log "Database ${GENEWEB_DB} not found, creating empty database..."
		cd ${GENEWEB_HOME}/share/data || exit 1
		${GENEWEB_HOME}/share/dist/gw/gwc -f -o ${GENEWEB_DB} > comm.log 2>&1
		cp ${GENEWEB_HOME}/share/dist/gw/a.gwf ${GENEWEB_DB_PATH}.gwf
		set_db_config "wizard_passwd_file" "wizard_passwd"
		echo "${GENEWEB_ADMIN}:${GENEWEB_ADMIN_PASS}" >> ${GENEWEB_HOME}/share/data/wizard_passwd
		gwlaunch_log "Configured access control."
		gwlaunch_log "-- ADMIN USERNAME: ${GENEWEB_ADMIN}"
		gwlaunch_log "-- ADMIN PASSWORD: ${GENEWEB_ADMIN_PASS}"
	else
		gwlaunch_log "Database ${GENEWEB_DB} exists."
	fi

	gwlaunch_log "Configuring settings for database ${GENEWEB_DB}"...
	for setting in $(env | grep GENEWEB_CONFIG_); do
		set_db_config "$(echo "${setting#GENEWEB_CONFIG_}" | cut -f1 -d=)" "$(echo "${setting#GENEWEB_CONFIG_}" | cut -f2 -d=)"
	done

	gwlaunch_log "Setting correct ownership of geneweb data."
	sudo chown -R geneweb:geneweb ${GENEWEB_HOME}/share/data
	sudo chown -R geneweb:geneweb ${GENEWEB_HOME}/etc
	sudo chown -R geneweb:geneweb ${GENEWEB_HOME}/log
	gwlaunch_log "-- Set ownership of geneweb data."

}

start() {
	cd ${GENEWEB_HOME}/share/data || exit 1

	gwlaunch_log "Starting gwsetup."
	${GENEWEB_HOME}/share/dist/gw/gwsetup \
	-daemon \
	-gd ${GENEWEB_HOME}/share/dist/gw \
	-only etc/gwsetup_only \
	>>${GENEWEB_HOME}/log/gwsetup.log 2>&1
	gwlaunch_log "-- Started gwsetup!"

	GWD_AUTH_FILE=${GWD_AUTH_FILE:=${GENEWEB_HOME}/etc/gwd_passwd}
	if [ -f $GWD_AUTH_FILE ]; then
		AUTH_ARG="-auth $GWD_AUTH_FILE"
	else
		AUTH_ARG=""
	fi

	gwlaunch_log "Starting gwd."
	${GENEWEB_HOME}/share/dist/gw/gwd \
	-daemon \
	-plugins -unsafe ${GENEWEB_HOME}/share/dist/gw/plugins \
	-trace_failed_passwd \
	-bd ${GENEWEB_BASE_PATH} \
	-hd ${GENEWEB_HOME}/share/dist/gw \
	-cache-in-memory ${GENEWEB_DB} \
	-log ${GENEWEB_HOME}/log/gwd.log \
	$AUTH_ARG 2>&1
	gwlaunch_log "-- Started gwd!"

	gwlaunch_log "Launch complete! -------------------------------------------------------"

	tail -f ${GENEWEB_HOME}/log/gwsetup.log | sed "s/^/$(date +%Y-%m-%d_%H:%M:%S) gwsetup: /" & \
	tail -f ${GENEWEB_HOME}/log/gwd.log | sed "s/^/$(date +%Y-%m-%d_%H:%M:%S) gwd: /"

}

init
start
