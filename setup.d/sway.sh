setup_sway() {
	log_info "Setting up sway"

	install_packages sway

	log_info "Stowing sway config"
	stow_repo "$HOME" sway
}
