#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="${DOTFILES:-$HOME/.dotfiles}"

log_info()
{
	echo ">> $*"
}

# Check if we can use the arch installer
ensure_arch() 
{
    if ! command -v pacman >/dev/null 2>&1; then
	    return 1
    else
	    return 0
    fi
}

# Check if we can use the fedora installer
ensure_fedora()
{
    if ! command -v dnf >/dev/null 2>&1; then
	    return 1
    else
	    return 0
    fi

}

ensure_os()
{
	if ! (ensure_fedora || ensure_arch) then
		log_info "Operating system not supported"
		exit 1
	fi
}

stow_arch_install()
{
	log_info "Installing stow (Arch)"
	sudo pacman -S --needed --noconfirm stow
}

stow_fedora_install()
{
	log_info "Installing stow (Fedora)"
	sudo dnf install stow
}

stow_install()
{
	if command -v stow >/dev/null 2>&1
	then
		return
	fi

	# Install differently depending on package manager
	if ensure_arch; then
		stow_arch_install
	elif ensure_fedora; then
		stow_fedora_install
	else
		log_info "Error: Operating system not supported. pacman or dnf are needed"
	fi

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

	if ensure_arch
	then
		sudo pacman -S --needed --noconfirm neovim
	elif ensure_fedora
	then
		sudo dnf install neovim
	fi
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


keyd_minimum()
{
	if ! command -v make >/dev/null 2>&1
	then
		log_info "Error: Make not installed"
		return
	fi

	if ! command -v systemctl >/dev/null 2>&1
	then
		log_info "Error: systemctl not found"
		return
	fi


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

sway_config()
{
	log_info "Stowing sway config"

	cd "$REPO_DIR"
	stow -Rvt "$HOME" sway
}

sway_install()
{
	if command -v sway >/dev/null 2>&1
	then
		log_info "Sway already installed"
		return
	fi

	log_info "Installing Sway"

	if ensure_arch
	then
		sudo pacman -S sway
	elif ensure_fedora
	then
		sudo dnf install sway
	fi
}

sway_setup()
{
	if ! command -v sway >/dev/null 2>&1
	then
		sway_install
	fi

	sway_config
}

main()
{
	ensure_os

	stow_install

	keyd_setup
	neovim_setup
}

main "$@"
