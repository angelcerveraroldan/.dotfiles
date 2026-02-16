keyd_install() {
	require_cmd git
	require_cmd make
	require_cmd systemctl

	log_info "Installing keyd"

	local tmpdir
	tmpdir="$(mktemp -d)"
	trap 'rm -rf "$tmpdir"' RETURN

	git clone https://github.com/rvaiya/keyd "$tmpdir/keyd"

	(
		cd "$tmpdir/keyd"
		make
		sudo make install
		sudo systemctl enable keyd
	)
}

keyd_config() {
	log_info "Stowing keyboard configuration"
	stow_repo "/" keyd sudo

	log_info "Restarting keyd to load the newly generated config"
	sudo systemctl restart keyd
}

setup_keyd() {
	if ! command_exists keyd; then
		keyd_install
	fi

	keyd_config
}
