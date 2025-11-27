#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="${DOTFILES:-$HOME/.dotfiles}"

log_info()
{
	echo ">> $*"
}

ensure_arch() 
{
    if ! command -v pacman >/dev/null 2>&1; then
        echo "ERROR: This setup script currently only supports Arch Linux (pacman not found)." >&2
        exit 1
    fi
}

stow_install()
{
	if command -v stow >/dev/null 2>&1
	then
		return
	fi

	log_info "Installing stow (Arch)"
	sudo pacman -S --needed --noconfirm stow

	if ! command -v stow >/dev/null 2>&1
	then
		echo "ERROR: Stow was not installed correctly."
		exit 1
	fi
}

neovim_install()
{
	if command -v nvim >/dev/null 2>&1
	then
		log_info "Neovim already installed"
		return
	fi

	log_info "Installing neovim (Arch)"
	sudo pacman -S --needed --noconfirm neovim
}

# Optionally install dependencies to make all features in neovim work.
#
# This installs:
# - ripgrep
# - fd
neovim_optional_deps()
{
	log_info "Installing optional Neovim dependencies (ripgrep, fd)"
    sudo pacman -S --needed --noconfirm ripgrep fd
}

neovim_setup()
{
	neovim_install
	neovim_config
}

neovim_config()
{
	log_info "Stowing neovim config"

	cd "$REPO_DIR"
	stow -Rvt "$HOME" nvim
}


keyd_install()
{
	log_info "Installing keyd"

	tmpdir="$(mktemp -d)"
	git clone https://github.com/rvaiya/keyd "$tmpdir/keyd"

	(
		cd "$tmpdir/keyd"
		make
		sudo make install
		sudo systemctl enable keyd
	)

	rm -rf "$tmpdir"
}

keyd_config()
{
	log_info "Stowing keyboard configuration"
	cd "$REPO_DIR"
	sudo stow -Rvt / keyd
	

	log_info "Restarting keyd to load the newly generated config"
	sudo systemctl restart keyd
}

keyd_setup()
{
	if ! command -v keyd >/dev/null 2>&1
	then
		keyd_install
	fi

	keyd_config
}

main()
{
	# For now, only arch works. Maybe add support for other OS later ...
	ensure_arch

	stow_install

	keyd_setup
	neovim_setup
}

main "$@"
