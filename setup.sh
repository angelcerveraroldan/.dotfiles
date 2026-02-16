#!/usr/bin/env bash
set -euo pipefail

readonly REPO_DIR="${DOTFILES:-$HOME/.dotfiles}"
readonly SETUP_DIR="$REPO_DIR/setup.d"

source "$SETUP_DIR/core.sh"
source "$SETUP_DIR/packages.sh"
source "$SETUP_DIR/emacs.sh"
source "$SETUP_DIR/neovim.sh"
source "$SETUP_DIR/sway.sh"
source "$SETUP_DIR/keyd.sh"

declare -a DEFAULT_COMPONENTS=(neovim emacs sway)
declare -a ONLY_COMPONENTS=()
declare -a SELECTED_COMPONENTS=()
declare -gA COMPONENT_VARIANTS=()

PULL=false

usage() {
	cat <<'EOF'
Usage: setup.sh [options]

Options:
  --pull                 Update dotfiles repo before setup (if online and clean)
  --only <list>           Comma-separated list of components to set up
  --only=emacs,neovim     Same as above
  --variant name=variant  Set component variant (repeatable)
  -h, --help              Show this help

Examples:
  ./setup.sh
  ./setup.sh --only emacs
  ./setup.sh --only neovim --variant neovim=nightly
EOF
}

validate_component() {
	local name="$1"
	local comp
	for comp in emacs neovim sway keyd; do
		if [[ "$comp" == "$name" ]]; then
			return 0
		fi
	done

	die "Unknown component: $name"
}

add_only_list() {
	local list="$1"
	local item
	local name
	local variant
	local -a items=()

	IFS=',' read -r -a items <<< "$list"

	for item in "${items[@]}"; do
		if [[ -z "$item" ]]; then
			continue
		fi

		if [[ "$item" == *@* ]]; then
			name="${item%%@*}"
			variant="${item#*@}"
			COMPONENT_VARIANTS["$name"]="$variant"
		else
			name="$item"
		fi

		validate_component "$name"
		ONLY_COMPONENTS+=("$name")
	done
}

add_variant() {
	local spec="$1"
	local name="${spec%%=*}"
	local variant="${spec#*=}"

	if [[ -z "$name" || -z "$variant" || "$name" == "$variant" ]]; then
		die "Invalid --variant value: $spec (expected name=variant)"
	fi

	validate_component "$name"
	COMPONENT_VARIANTS["$name"]="$variant"
}

parse_args() {
	while [[ "$#" -gt 0 ]]; do
		case "$1" in
			--pull)
				PULL=true
				;;
			--only)
				shift
				[[ "$#" -gt 0 ]] || die "--only requires a value"
				add_only_list "$1"
				;;
			--only=*)
				add_only_list "${1#*=}"
				;;
			--variant)
				shift
				[[ "$#" -gt 0 ]] || die "--variant requires name=variant"
				add_variant "$1"
				;;
			--variant=*)
				add_variant "${1#*=}"
				;;
			-h|--help)
				usage
				exit 0
				;;
			*)
				die "Unknown argument: $1"
				;;
		esac
		shift
	done
}

build_component_list() {
	local -a input=()
	local -A seen=()
	local comp

	SELECTED_COMPONENTS=()

	if [[ "${#ONLY_COMPONENTS[@]}" -gt 0 ]]; then
		input=("${ONLY_COMPONENTS[@]}")
	else
		input=("${DEFAULT_COMPONENTS[@]}")
	fi

	for comp in "${input[@]}"; do
		if [[ -z "${seen[$comp]-}" ]]; then
			SELECTED_COMPONENTS+=("$comp")
			seen["$comp"]=1
		fi
	done
}

pull_latest() {
	require_cmd git

	if ! git -C "$REPO_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
		die "Not a git repository: $REPO_DIR"
	fi

	if ! has_internet; then
		log_info "Skipping git pull (no internet)"
		return
	fi

	if [[ -n "$(git -C "$REPO_DIR" status --porcelain)" ]]; then
		log_info "Skipping git pull (uncommitted changes)"
		return
	fi

	log_info "Updating dotfiles repo"
	git -C "$REPO_DIR" pull --ff-only
}

run_component() {
	case "$1" in
		emacs)
			setup_emacs
			;;
		neovim)
			setup_neovim
			;;
		sway)
			setup_sway
			;;
		keyd)
			setup_keyd
			;;
		*)
			die "Unknown component: $1"
			;;
	esac
}

main() {
	parse_args "$@"
	init_pkg_manager
	stow_install

	if [[ "$PULL" == "true" ]]; then
		pull_latest
	fi

	build_component_list

	local comp
	for comp in "${SELECTED_COMPONENTS[@]}"; do
		run_component "$comp"
	done
}

main "$@"
