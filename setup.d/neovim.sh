setup_neovim() {
	log_info "Setting up neovim"

	local pkg="neovim"
	local variant="${COMPONENT_VARIANTS[neovim]-}"
	if [[ -n "$variant" ]]; then
		pkg+="@${variant}"
	fi

	if [[ -z "$variant" ]] && command_exists nvim; then
		log_info "Neovim already installed; skipping package install"
	else
		install_packages "$pkg"
	fi

	log_info "Stowing neovim config"
	stow_repo "$HOME" nvim
}
