log_info() {
	printf '>> %s\n' "$*"
}

log_error() {
	printf 'ERROR: %s\n' "$*" >&2
}

log_warn() {
	printf 'WARN: %s\n' "$*" >&2
}

die() {
	log_error "$*"
	exit 1
}

command_exists() {
	command -v "$1" >/dev/null 2>&1
}

require_cmd() {
	command_exists "$1" || die "Missing required command: $1"
}

has_internet() {
	if command_exists curl; then
		curl -fsSL https://github.com >/dev/null 2>&1
		return $?
	fi

	if command_exists ping; then
		ping -c 1 -W 1 1.1.1.1 >/dev/null 2>&1
		return $?
	fi

	return 1
}

stow_repo() {
	local target="$1"
	local package="$2"
	local use_sudo="${3:-}"

	(
		cd "$REPO_DIR"
		if [[ "$use_sudo" == "sudo" ]]; then
			sudo stow -Rvt "$target" "$package"
		else
			stow -Rvt "$target" "$package"
		fi
	)
}
