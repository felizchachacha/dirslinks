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

function parter_a_pair() {
	local existing_partner="${1}"
	local new_partner="${2}"
	local mountpoint=$("${HELPERUTILSDIR}"/get-mountpoint.bash "${existing_partner}")
	if [[ $("${HELPERUTILSDIR}"/get-mountpoint.bash "${new_partner}") != "${mountpoint}" ]]; then
		echo "'${existing_partner}' and '${new_partner}' must be on same device"
		exit 2
	fi
	existing_partner=$(realpath --relative-to="${mountpoint}" "${existing_partner}")
	new_partner=$(realpath --relative-to="${mountpoint}" "${new_partner}")
	local blockdev=$("${HELPERUTILSDIR}"/get-filesdev.bash "${mountpoint}")
	echo "link \"existing_partner}\" \"${new_partner}\"" | debugfs -w "${blockdev}"
}

function partner_them() {
	local existing_partner="${1}"
	shift
	local new_partner
	[[ "${existing_partner}" == '' ]] && exit 1
	while [ ${#} -gt 0 ]; do
		new_partner="${1}"
		[[ "${new_partner}" == '' ]] && exit 1
		shift
		parter_a_pair "${existing_partner}" "${new_partner}"
	done
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
		exit 0
		;;

		'tag')
			shift
			ln "${@}"
		exit 0
		;;


		'init'|'i')
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
			if [[ "${2}" == '' ]]; then
				mount -v "${WHAT}"
				exit 0
			fi
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

		'remount'|'rmnt'|'refresh'|'re')
			shift
			umount "${@}" || true
			mount "${@}"
		exit 0
		;;

		*)
			echo what?
			echo "Example could be like ./dt.bash -v init /dev/zvol/rpool/volumes/debugfs -d /home/ubuntu-studio/Attention/"
			shift
		exit 1
		;;

	esac
done
