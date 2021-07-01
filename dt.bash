#!/usr/bin/env bash

set -e
readonly DEBUG=${DEBUG:-false}
${DEBUG} && set -x

readonly ME=$(realpath $(which "${0}"))
readonly MYREALDIR=$(dirname "${ME}")
readonly FST='ext4'
readonly HELPERUTILSDIR="${MYREALDIR}"/../s/exe # assumed https://gitlab.com/8e/s is there
declare -a MkFsOpts=(-e continue -E discard -E enable_periodic_fsck=0 -E norecovery -E discard -O sparse_super -O large_file -O filetype -O resize_inode -O dir_index -O ext_attr)
readonly MNT_DEFOPTS="norecovery,acl,discard,user_xattr"

[ ${#} -eq 0 ] && set -- ls

function partner_them() {
	true	
}

while [ ${#} -gt 0 ]; do
	case ${1} in

		'-v')
			shift # past param
			set -x
		;;

		'-L')
			shift # past param
			readonly LABEL=$1
			shift # past value
		;;

		'partner')
			shift
			partner_them "${@}"
			echo "" | debugfs -w "${BLOCKDEV}"
		;;

		''|'create-')
			
		;;

		'init')
			shift
			if  [ -b $(realpath "${1}") ]; then
				readonly BLOCKDEV="${1}"
				shift
				readonly DT_BLOCKSZ=${DT_BLOCKSZ:-$(blockdev --getioopt "${BLOCKDEV}")}
			else
				readonly DT_BLOCKSZ=$(getconf PAGESIZE)
				readonly BLOCKDEV=$("${HELPERUTILSDIR}"/zvol-create.bash $(basename "${1}") -b "${DT_BLOCKSZ}")
				shift
			fi
			NkFsOpts+=(-b "${DT_BLOCKSZ}")
			"${HELPERUTILSDIR}"/is_zvol.bash "${BLOCKDEV}" && NkFsOpts+=(-O ^has_journal -J size=0)
			[[ "${LABEL}" == '' ]] && readonly LABEL=$(basename "${BLOCKDEV}")
			NkFsOpts+=(-L "${LABEL}")
			mkfs."${FST}" "${MkFsOpts[@]}" "${@}" "${BLOCKDEV}"
		exit 0
		;;

		'mount')
			shift
			readonly WHAT="${1}"
			shift
			readonly MOUNTPOINT=$("${HELPERUTILSDIR}"/ensure-path.bash "${1}")
			shift
			# the rest are mount options on fstab format
			readonly OPTS="${MNT_DEFOPTS},${@}"
			#<file system> <mount point> <type> <options> <dump> <pass>
			readonly FSTAB_LINE="${WHAT}\t${MOUNTPOINT}\t${FST}\t${OPTS}\t0\t0"
			if ! grep -- "^${FSTAB_LINE}$" /etc/fstab; then
				cp /etc/fstab /var/backups/
				sed -i.bak "\|^${WHAT}.*$|d" /etc/fstab
				echo -e "${FSTAB_LINE}" | tee -a /etc/fstab
			fi
			dmesg -HW &
			readonly BGP=$!
			if ! mount -v "${WHAT}"; then
				readonly EXCODE=$?
				sleep 1
				kill -TERM "${BGP}"
				exit "${EXCODE}"
			else
				kill -TERM "${BGP}"
				mount | grep -- "${WHAT}"
			fi
		exit 0
		;;

		'unmount')
			shift
			umount "${@}"
		exit 0
		;;

		*)
			echo what?
			shift
		exit 1
		;;

	esac
done
