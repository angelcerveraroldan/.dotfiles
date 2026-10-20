setup_niri() {
	log_info "Setting up niri"

	install_packages niri dms

	log_info "Stowing niri config"
	stow_repo "$HOME" niri

	log_info "Binding DMS to niri service"
	systemctl --user add-wants niri.service dms
}
