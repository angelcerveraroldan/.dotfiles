PKG_MGR=""

declare -A PKG_MAP_PACMAN=(
	[git]=git
	[pkgconf]=pkgconf
	[hunspell]=hunspell
	[hunspell-en]=hunspell-en_GB
	[stow]=stow
	[neovim]=neovim
	[neovim@nightly]=aur:neovim-git
	[ripgrep]=ripgrep
	[fd]=fd
	[sway]=sway
	[emacs]=emacs
	[enchant]=enchant
	[libvterm]=libvterm
	[cmake]=cmake
	[libtool]=libtool
	[make]=make
	[gcc]=gcc
	[sqlite]=sqlite
	[rust]=rust
	[rust-analyzer]=rust-analyzer
	[go]=go
	[gopls]=gopls
)

declare -A PKG_MAP_DNF=(
	[git]=git
	[pkgconf]=pkgconf-pkg-config
	[hunspell]=hunspell
	[hunspell-en]=hunspell-en-GB
	[stow]=stow
	[neovim]=neovim
	[neovim@nightly]=neovim-nightly
	[ripgrep]=ripgrep
	[fd]=fd-find
	[sway]=sway
	[emacs]=emacs
	[enchant]=enchant2
	[libvterm]="libvterm libvterm-devel"
	[cmake]=cmake
	[libtool]=libtool
	[make]=make
	[gcc]=gcc
	[sqlite]=sqlite
	[rust]=rust
	[rust-analyzer]=rust-analyzer
	[go]=golang
	[gopls]=golang-x-tools-gopls
)

declare -A PKG_MAP_APT=(
	[git]=git
	[pkgconf]=pkg-config
	[hunspell]=hunspell
	[hunspell-en]=hunspell-en-gb
	[stow]=stow
	[neovim]=neovim
	[neovim@nightly]=snap:nvim/edge
	[ripgrep]=ripgrep
	[fd]=fd-find
	[sway]=sway
	[emacs]=emacs
	[enchant]=enchant-2
	[libvterm]=libvterm-dev
	[cmake]=cmake
	[libtool]=libtool
	[make]=make
	[gcc]=gcc
	[sqlite]=sqlite3
	[rust]="rustc cargo"
	[rust-analyzer]=rust-analyzer
	[go]=golang-go
	[gopls]=golang-gopls
)

init_pkg_manager() {
	if command_exists pacman; then
		PKG_MGR="pacman"
		return
	fi

	if command_exists dnf; then
		PKG_MGR="dnf"
		return
	fi

	if command_exists apt-get; then
		PKG_MGR="apt"
		return
	fi

	die "Operating system not supported. pacman, dnf, or apt-get are needed."
}

pkg_map_lookup() {
	local logical="$1"
	local mapped=""

	case "$PKG_MGR" in
		pacman)
			mapped="${PKG_MAP_PACMAN[$logical]-}"
			;;
		dnf)
			mapped="${PKG_MAP_DNF[$logical]-}"
			;;
		apt)
			mapped="${PKG_MAP_APT[$logical]-}"
			;;
		*)
			mapped=""
			;;
	esac

	if [[ -z "$mapped" ]]; then
		die "No package mapping for '$logical' on $PKG_MGR"
	fi

	printf '%s' "$mapped"
}

aur_helper() {
	if command_exists yay; then
		printf 'yay'
		return 0
	fi

	if command_exists paru; then
		printf 'paru'
		return 0
	fi

	return 1
}

pkg_command_candidates() {
	case "$1" in
		stow)
			printf 'stow'
			;;
		neovim)
			printf 'nvim'
			;;
		emacs)
			printf 'emacs'
			;;
		git)
			printf 'git'
			;;
		ripgrep)
			printf 'rg'
			;;
		fd)
			printf 'fd fdfind'
			;;
		sway)
			printf 'sway'
			;;
		pkgconf)
			printf 'pkgconf pkg-config'
			;;
		hunspell)
			printf 'hunspell'
			;;
		sqlite)
			printf 'sqlite3'
			;;
		cmake)
			printf 'cmake'
			;;
		libtool)
			printf 'libtool'
			;;
		make)
			printf 'make'
			;;
		gcc)
			printf 'gcc'
			;;
		rust)
			printf 'rustc'
			;;
		rust-analyzer)
			printf 'rust-analyzer'
			;;
		go)
			printf 'go'
			;;
		gopls)
			printf 'gopls'
			;;
	esac
}

find_existing_command() {
	local cmd
	for cmd in "$@"; do
		if command_exists "$cmd"; then
			printf '%s' "$cmd"
			return 0
		fi
	done

	return 1
}

should_warn_on_install_failure() {
	local output="$1"
	local lower="${output,,}"

	case "$lower" in
		*"conflicts with"*|*"are in conflict"*|*"conflicts detected"*|*"conflicting files"*|*"exists in filesystem"*|*"already installed"*|*"is up to date"*|*"nothing to do"*)
			return 0
			;;
	esac

	return 1
}

run_install_command() {
	local label="$1"
	shift
	local output_file
	local output

	output_file="$(mktemp)"

	if "$@" 2>&1 | tee "$output_file"; then
		rm -f "$output_file"
		return 0
	fi

	output="$(cat "$output_file")"
	rm -f "$output_file"

	if should_warn_on_install_failure "$output"; then
		log_warn "$label failed due to conflicts; skipping"
		return 0
	fi

	die "$label failed"
}

install_repo_packages() {
	local pkg
	for pkg in "$@"; do
		case "$PKG_MGR" in
			pacman)
				run_install_command "pacman install $pkg" sudo pacman -S --needed --noconfirm "$pkg"
				;;
			dnf)
				run_install_command "dnf install $pkg" sudo dnf install "$pkg"
				;;
			apt)
				run_install_command "apt-get install $pkg" sudo apt-get install -y "$pkg"
				;;
			*)
				die "Unsupported package manager: $PKG_MGR"
				;;
		esac
	done
}

install_aur_packages() {
	local helper
	if ! helper="$(aur_helper)"; then
		die "AUR helper not found (install yay or paru)"
	fi

	local pkg
	for pkg in "$@"; do
		run_install_command "AUR install $pkg" "$helper" -S --needed --noconfirm "$pkg"
	done
}

install_snap_packages() {
	if ! command_exists snap; then
		if [[ "$PKG_MGR" == "apt" ]]; then
			log_info "Installing snapd"
			install_repo_packages snapd
		else
			die "snap not found (install snapd)"
		fi
	fi

	local item
	local name
	local channel
	for item in "$@"; do
		name="${item%%/*}"
		if [[ "$item" == "$name" ]]; then
			run_install_command "snap install $name" sudo snap install "$name"
		else
			channel="${item#*/}"
			run_install_command "snap install $name ($channel)" sudo snap install "$name" --channel="$channel"
		fi
	done
}

install_packages() {
	if [[ -z "${PKG_MGR}" ]]; then
		die "Package manager not initialized."
	fi

	if [[ "$#" -eq 0 ]]; then
		die "No packages requested."
	fi

	local logical
	local mapped
	local -a resolved=()
	local -a display=()
	local -a parts=()
	local -a cmd_parts=()
	local -a repo_pkgs=()
	local -a aur_pkgs=()
	local -a snap_pkgs=()
	local item
	local cmd
	local cmds

	for logical in "$@"; do
		if [[ "$logical" != *@* ]]; then
			cmds="$(pkg_command_candidates "$logical")"
			if [[ -n "$cmds" ]]; then
				read -r -a cmd_parts <<< "$cmds"
				if cmd="$(find_existing_command "${cmd_parts[@]}")"; then
					log_info "Skipping $logical (command exists: $cmd)"
					cmd_parts=()
					continue
				fi
				cmd_parts=()
			fi
		fi

		mapped="$(pkg_map_lookup "$logical")"
		read -r -a parts <<< "$mapped"
		resolved+=("${parts[@]}")

		if [[ "$mapped" == "$logical" ]]; then
			display+=("$logical")
		else
			display+=("$logical->$mapped")
		fi
		parts=()
	done

	if [[ "${#resolved[@]}" -eq 0 ]]; then
		log_info "All requested packages already available"
		return 0
	fi

	for item in "${resolved[@]}"; do
		case "$item" in
			aur:*)
				aur_pkgs+=("${item#aur:}")
				;;
			snap:*)
				snap_pkgs+=("${item#snap:}")
				;;
			*)
				repo_pkgs+=("$item")
				;;
		esac
	done

	log_info "Installing packages: ${display[*]}"

	if [[ "${#repo_pkgs[@]}" -gt 0 ]]; then
		install_repo_packages "${repo_pkgs[@]}"
	fi

	if [[ "${#aur_pkgs[@]}" -gt 0 ]]; then
		if [[ "$PKG_MGR" != "pacman" ]]; then
			die "AUR packages requested on non-pacman system"
		fi
		install_aur_packages "${aur_pkgs[@]}"
	fi

	if [[ "${#snap_pkgs[@]}" -gt 0 ]]; then
		install_snap_packages "${snap_pkgs[@]}"
	fi
}

stow_install() {
	if command_exists stow; then
		return
	fi

	log_info "Installing stow"
	install_packages stow

	command_exists stow || die "Stow was not installed correctly."
}
