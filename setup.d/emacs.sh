setup_emacs() {
	log_info "Setting up emacs"

	local pkg="emacs"
	local variant="${COMPONENT_VARIANTS[emacs]-}"
	if [[ -n "$variant" ]]; then
		pkg+="@${variant}"
	fi

	local -a deps=(
		"$pkg"
		enchant
		pkgconf
		hunspell
		hunspell-en
		git
		ripgrep
		sqlite
		libvterm
		cmake
		libtool
		make
		gcc
		rust
		rust-analyzer
		go
		gopls
	)

	install_packages "${deps[@]}"

	log_info "Stowing emacs config"
	stow_repo "$HOME" emacs

	if command_exists systemctl; then
		log_info "Enabling emacs daemon"
		if ! systemctl --user daemon-reload; then
			log_warn "Failed to reload user systemd"
			return
		fi

		if ! systemctl --user enable --now emacs.service; then
			log_warn "Failed to enable emacs daemon"
		fi
	else
		log_warn "systemctl not found; skipping emacs daemon enable"
	fi
}
