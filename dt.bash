#!/usr/bin/env bash

set -e
readonly DEBUG=${DEBUG:-false}
${DEBUG} && set -x

readonly ME=$(realpath $(which "${0}"))
readonly MYREALDIR=$(dirname "${ME}")

[ ${#} -eq 0 ] && set -- ls

function partner_them() {
	
}

while [ ${#} -gt 0 ]; do
	case ${1} in

		'-v')
			shift # past param
			set -x
		;;

		'partner')
			shift # past param
			partner_them "${@}"
		;;

		''|'create-')
			
		;;

		*)
			echo what?
		;;

	esac
done
