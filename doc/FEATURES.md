# Scripts Collection Feature Reference

This document provides a user-facing reference for the features included in the `scripts` repository.

This repository is not a single application. It is a collection of independent utilities, system setup installers, operational jobs intended for cron execution, and the configuration files and dotfiles used by those tools.

The README explains the overall purpose of the repository, installation, and layout conventions. The header documentation in each executable file explains the exact Usage, Options, Requirements, Exit Status, and Version History for that file.

This `FEATURES.md` sits between those two layers.

Its purpose is to make it possible to answer the following question without opening every source file individually:

"What exists in this repository, and which script should I use to do a particular task?"

The implementation and the header documentation in each script are the ultimate source of truth. This document is a user-facing index for discovering those capabilities.


## 1. Repository Feature Structure

The user-facing features in `scripts` are broadly divided into the following layers.

| Layer | Role | Typical use |
| --- | --- | --- |
| Repository root | Independent utilities intended to be run directly | Manual, task-oriented commands |
| `installer/` | Setup scripts for operating systems, middleware, development environments, monitoring, and security | Explicit machine or environment setup |
| `cron/bin/` | Operational jobs intended to run unattended | Scheduled execution |
| `cron/etc/` | Configuration files, cron triggers, and logrotate definitions for scheduled jobs | Supports `cron/bin/` deployments |
| `etc/` | Configuration files, templates, and data used by top-level scripts and installers | Site-specific or supporting configuration |
| `dot_files/` | Dotfiles deployed into user environments | User environment setup |
| `test/` | Tests used to validate the repository | Repository validation |
| `doc/` | Policies, version history, licenses, and feature references | Documentation |


## 2. Top-Level Scripts

Scripts located at the repository root are, in principle, independent commands intended to be invoked directly by users.

The current repository contains 92 such scripts.

### Top-level capability index

| Area | Representative commands | Main purpose |
| --- | --- | --- |
| HTML, writing, and content transformation | `add_hr_h2.py`, `aozora_prepare.rb`, `fix_anchor.py`, `format_html.py`, `html2yaml.py` | Format, normalize, and transform text or HTML |
| Japanese input and dictionaries | `anthy_create_aa_dic.sh`, `convert_msime2canna.rb` | Build and convert Japanese input dictionaries |
| Apache, web servers, and WordPress | `apache_log_analysis.sh`, `apache_calculater.py`, `apache_blog_analysis.py`, `wp_cachectl.py` | Analyze web traffic and operate WordPress caches |
| Package management | `apt-upgrade.sh`, `brew-upgrade.sh`, `dpkg-hold.sh` | Update packages and control package state |
| Files, directories, and filesystems | `chmodtree.py`, `cleanup-junk-files.sh`, `dirsize.py`, `find_range.py`, `flatdirs.py` | Inspect, transform, clean, and normalize filesystems |
| Zsh history | `erase_history.py`, `filter_history.sh` | Remove or filter shell history entries |
| Shell environment maintenance | `fix_compinit.sh`, `unfix_compinit.sh`, `xmap.sh` | Maintain Zsh and X11 user configuration |
| Block devices and encrypted volumes | `get-device.sh`, `get-mountpoint.sh`, `get-serial.sh`, `tcmount.py`, `sd_extract.sh` | Resolve storage devices and operate removable or encrypted storage |
| Git repository management | `git-all-pull.sh`, `git-archive-repo.sh`, `git-create-repo.sh`, `git-symlink.sh`, `remove-repo.sh` | Update, archive, create, link, and remove repositories |
| Networking and transfers | `pyping.py`, `wakeonlan.py`, `wget.py`, `wget.rb`, `send_files.sh` | Check connectivity, download data, wake hosts, and transfer files |
| Dashcam, GPX, and Instagram | `dashcam_sync.sh`, `gpx_sync.sh`, `insta_downloader.py`, `insta_sync.sh` | Synchronize and organize personal media or location data |
| Fastladder | `fav-pins-on-fastladder.sh`, `get-fastladder-db.sh`, `get-feeds-from-fastladder.sh`, `vacuum-fastladder-db.sh` | Maintain Fastladder data and databases |
| Development and testing | `check_header_doc.py`, `find_pycompat.py`, `pyck.py`, `run_tests.sh`, `setup_scripts.sh` | Validate source code and repository conventions |
| System administration | `check_reboot.sh`, `check_sshd_config.sh`, `get_resources.sh`, `server_alive_check.sh` | Inspect system state, services, and availability |
| Other utilities | `namecalc.py`, `namecalc.rb`, `simple_passwd.py`, `simple_passwd.rb` | Small standalone utilities |


## 3. HTML, Writing, and Content Transformation

### add_hr_h2.py

Inserts an `<hr>` immediately before each `<h2>` element in HTML.

If an `<hr>` already exists immediately before the `<h2>`, it does not add a duplicate.

It can update the input file in place or write the result to a separate output file.


### aozora_prepare.rb

Prepares Aozora Bunko text for subsequent processing.

It performs operations such as encoding conversion, annotation removal, full-width space handling, and newline normalization.


### fix_anchor.py

Normalizes the position of reference links relative to Japanese full stops in HTML or Markdown.

For example, it rewrites:

    。<a href="#ref1">[1]</a>

into:

    <a href="#ref1">[1]</a>。


### format_html.py

Formats HTML into a readable, consistently indented structure.

Its main behaviors include:

- Indenting according to HTML nesting
- Collapsing simple text-only elements onto one line
- Preserving the contents of `pre`, `script`, `style`, and `textarea`
- Preserving code contents
- Normalizing excessive blank lines
- Avoiding unnecessary line breaks around inline elements

It is a lightweight formatter, not an HTML validator or repair tool.


### html2yaml.py

Converts HTML content into a form that can be handled as YAML.


### remove_space_eol.sh

Removes unnecessary trailing whitespace from lines.


### zero_padding.py

Applies zero padding to numeric strings or file names.


### platex2pdf.sh

Wraps the workflow required to generate a PDF from a pLaTeX document.


## 4. Japanese Input and Dictionary Conversion

### anthy_create_aa_dic.sh

Creates a custom Anthy dictionary for ASCII Art entries.

It converts MS-IME dictionary data and feeds it into the process required to create a dictionary usable by Anthy.


### convert_msime2canna.rb

Converts a Microsoft IME dictionary into Canna format.

It specifically targets entries categorized as "顔文字".


## 5. Apache, Web Servers, and WordPress

### apache_log_analysis.sh

Analyzes Apache SSL access logs.

It primarily reports:

- Accessed URLs
- Referrers
- User-Agent values
- Browser counts
- Daily access counts
- Access counts by time
- Recent accesses
- Recent referrers

IPs listed in `apache_ignore.list` are excluded.

Blog-entry-specific page-view estimation is intentionally not implemented here; that responsibility belongs to `apache_blog_analysis.py`.


### apache_calculater.py

Aggregates Apache log data to calculate hit counts per IP address and the percentage of client cache hits.

It supports gzip-compressed logs.

When multiple log files are specified, their results are aggregated together.


### apache_blog_analysis.py

Analyzes WordPress article access without treating a simple HTTP 200 count as the only page-view measure.

It separates three distinct metrics:

- Candidate page views
- Asset-confirmed views
- Estimated sessions

Asset-confirmed views use related requests for theme, plugin, upload, or WordPress core assets as supporting evidence that page rendering progressed.

However, browser caches, Service Workers, CDNs, Referrer-Policy behavior, and similar mechanisms can suppress such requests, so these metrics must not be treated as exact human visitor counts.


### wp_cachectl.py

Uses WP-CLI to perform event-driven WordPress cache operations.

Rather than clearing caches periodically without cause, it treats cache clearing as an operation performed at event boundaries such as:

- After a WordPress update
- After a theme change
- During troubleshooting

The cache layers used by this tool are:

- L1: WordPress Transients
- L2: Object Cache
- L3: Page Cache Plugin
- L4: CDN / Browser Cache

L4 is not automatically cleared; the tool provides guidance only.


## 6. Package Updates and Package Management

### apt-upgrade.sh

Runs the grouped package update and upgrade workflow for Debian-based systems using APT.


### brew-upgrade.sh

Runs the grouped package update and upgrade workflow for Homebrew environments.


### dpkg-hold.sh

Controls Debian package hold states in order to prevent specific packages from being upgraded automatically.


## 7. Files, Directories, and Filesystems

### chmodtree.py

Recursively normalizes permissions, owners, and groups across a directory tree.

By default, it changes only entries that differ from the configured state.

It can also perform forced changes and modify symlink ownership when requested.


### cleanup-junk-files.sh

Recursively removes common junk files from a specified directory.

Targets include:

- `.DS_Store`
- AppleDouble `._*` files
- `*.un~`
- `__pycache__` directories


### clear_chromium_cache.sh

Removes Chromium's `Web Data` directory.

It does not forcibly terminate Chromium processes, so Chromium should be closed before running it.


### cltmp.sh

Removes temporary and cached files on Unix-like systems.

It supports both macOS and Linux.

In particular, `$HOME/.cache` is treated as disposable cache storage and its contents are not preserved.

Different targets are handled according to retention tiers of 0, 1, 7, or 30 days.


### dirsize.py

Lists files and directories immediately under a specified directory and displays the total size of the files directly contained there in human-readable form.

It is not intended to recursively sum the size of contents inside subdirectories.


### du.py

Provides simplified disk-usage reporting for macOS.

It offers a depth control similar to GNU `du --max-depth` and can include or exclude hidden directories.


### els.py

Provides a detailed file listing similar to `ls -l` but includes additional timestamp information.

It reports items such as:

- Permissions
- Size
- Owner
- Group
- atime
- mtime
- ctime
- Birth time


### exif_drop.sh

Removes EXIF metadata from images or similar files.


### find_range.py

Searches a directory tree for files modified within a specified datetime range.

UTC is used by default, with an option to use local time instead.

Hidden directories are excluded by default.


### flatdirs.py

Flattens a directory hierarchy.

Files can be collected into a base directory using:

- Move
- Copy
- Rename

When running in a mode that actually changes files, it displays the current directory and asks for confirmation.


### image_resize.py

Resizes image files according to specified conditions.


### list_file_counts.py

Counts and reports files in a directory tree.


### md5.py

Calculates the MD5 digest of input data or files.


### png_info.py

Inspects and reports the structure and metadata of PNG files.


### swapext.py

Changes file extensions from one extension to another.


### tree.sh

Displays a directory hierarchy in tree form.


### unixtime2date.py

Converts between Unix timestamps and human-readable datetime representations.


### unzip_subdir.py

Safely extracts part of a ZIP archive into a specified directory.


## 8. Zsh History

### erase_history.py

Removes the most recent N entries from `~/.zsh_history`.

It is intended for quickly removing mistyped or otherwise unwanted recent commands from shell history.

If the last history entry is the invocation of `erase_history.py` itself, that invocation is preserved and the preceding target entries are removed instead.


### filter_history.sh

Removes Zsh history entries containing a specified pattern.

It creates a backup before modifying the history file and reports both the number of removed entries and a diff afterward.


## 9. Zsh and Shell Environment Maintenance

### fix_compinit.sh

Fixes ownership and permissions on Homebrew Zsh completion directories on macOS in order to resolve:

    compinit: insecure directories

It normalizes the target directories to `root:wheel` ownership and removes insecure write permissions.


### unfix_compinit.sh

Provides the counterpart operation for reverting ownership and permission changes applied by `fix_compinit.sh`.


### xmap.sh

Loads the user's `$HOME/.Xmodmap` through `xmodmap` when both the command and the file are available.


## 10. Block Devices, Mounts, and Encrypted Volumes

### get-device.sh

Resolves a mountpoint to its underlying base block device.

It follows partitions and device-mapper layers and prints a single base-device path such as:

    /dev/sdc


### get-mountpoint.sh

Resolves a block-device path to the mountpoint where it is currently mounted.

It supports base disks, partitions, and device-mapper paths and uses `findmnt` and `lsblk`.


### get-serial.sh

Retrieves the disk serial number for a `/dev/*` block-device path.

It does not resolve mountpoints.

When needed, it can be composed with:

    get-device.sh
    get-serial.sh


### tcmount.py

Assists with mounting and unmounting TrueCrypt or VeraCrypt encrypted devices.

It supports device selection, filesystem and encoding options, TrueCrypt or VeraCrypt selection, external containers, and an explicit mount target.


### sd_extract.sh

Copies files from multiple source directories on an SD card to a local destination when they match configured file patterns.

Sources, patterns, destination, and permissions are read from an external configuration file.

If some file copies fail, processing continues for the remaining files, and failures are reported at the end.


## 11. Git Repository Management

### git-all-pull.sh

Updates multiple local Git repositories in bulk.

Target trees can include:

- github
- git
- www

It supports pruning deleted remote-tracking branches, creating home-directory symlinks, and dry-run mode.

For remote branches, it also supports:

    --list-remote

to preview branches other than `master` and `main`, and:

    --delete-remote-branches

to delete them.

`--hard` performs `git reset --hard` and is therefore destructive.


### git-archive-repo.sh

Creates compressed archives of multiple local Git repository directories.

Source directories and archive output paths are read from `git-archive-repo.conf`.


### git-co-remote-branch.sh

Creates a local branch for a branch that currently exists only on the remote and configures it to track the corresponding remote branch.


### git-create-repo.sh

Creates and deletes Git repositories.

The default repository root is `/var/lib/git`.

It supports a custom path, dry-run mode, explicit sudo control, and owner/group selection.


### git-follow-origin.sh

Assists with a merge workflow that incorporates changes from a specified GitHub repository into the local master branch.


### git-symlink.sh

Creates or recreates symlinks in `$HOME` corresponding to repositories directly under:

    $HOME/local/github
    $HOME/local/git

In normal sync mode, it also removes broken symlinks directly under `$HOME`.

In uninstall mode, it removes symlinks whose names correspond to target repositories.


### remove-repo.sh

Removes specified repositories from:

    $HOME/local/github
    $HOME/local/git

and removes the corresponding `$HOME` symlinks.

The default behavior is dry-run.

Actual deletion occurs only when:

    -x

is specified.

The script verifies that the target is a Git repository before removing it.


## 12. Networking, Transfers, and Remote Operations

### pyping.py

Performs ping-like network reachability checks from Python.


### wakeonlan.py

Sends Wake-on-LAN magic packets.


### wget.py

Provides a Python implementation of a download utility.


### wget.rb

Provides a Ruby implementation of a download utility.


### send_files.sh

Packages a directory into a password-protected archive.

It generates a secure random password and creates both the archive and password information in a temporary area.

By default, the archive is copied to a configured output directory.

When `--send` is specified, it sends the archive through Gmail using `mail` and `uuencode`.

It also supports a 7-Zip mode.


### gpg-import.sh

Wraps the workflow required to import GPG keys.


## 13. Dashcam, GPX, and Instagram

### dashcam_sync.sh

Synchronizes dashcam files from a local source to an external drive and organizes them into a year-based directory structure.

Source and destination locations are specified in `dashcam_sync.conf`.


### gpx_sync.sh

Organizes, copies, and remotely synchronizes GPX files.

Directories, remote hosts, permissions, and related settings are read from `gpx_sync.conf`.

It also supports rsync to multiple remote hosts.


### insta_downloader.py

Downloads target Instagram content.


### insta_video_downloader.py

Downloads Instagram videos.


### insta_sync.sh

Synchronizes Instagram-related data according to configuration.


### insta_update.sh

Runs the Instagram-related update workflow.


## 14. Fastladder

### fav-pins-on-fastladder.sh

Manages Fastladder favorite pins.

Its operations include:

- Removing feeds with zero subscribers
- Extracting favorite pinned links
- Removing existing pins


### get-fastladder-db.sh

Retrieves the Fastladder database from a remote server using rsync.

A backup of the local database is created before it is overwritten.


### put-fastladder-db.sh

Transfers the local Fastladder database to a remote server.


### get-feeds-from-fastladder.sh

Retrieves the following from the Fastladder SQLite database:

- A list of feed titles
- The total feed count


### vacuum-fastladder-db.sh

Performs database cleanup and VACUUM operations on the Fastladder SQLite database.


## 15. Development, Testing, and Repository Maintenance

### check_header_doc.py

Mechanically enforces the repository-wide header documentation policy.

It detects issues such as:

- Blank lines in the header block without comment markers
- Typos such as `##`
- Non-comment lines accidentally placed inside the header block

It does not depend on VCS metadata and can be used from CI or cron.


### find_pycompat.py

Checks which Python versions are compatible with Python source code.


### pyck.py

Combines linting, formatting, and import cleanup for Python code.

It primarily uses:

- flake8
- autopep8
- autoflake
- isort

It supports both dry-run and auto-fix modes.


### run_tests.sh

Runs the repository test suite against specified Python and Ruby environments.

For repository-wide nightly testing, `cron/bin/run_tests` invokes this script across multiple interpreter versions and also performs additional repository-wide checks.


### setup_scripts.sh

Applies the required executable permissions to executable scripts in the repository.


### show_version.py

Displays the versions of major commands, languages, and tools available on the system.


## 16. System Administration Utilities

### cal.py

Acts as a wrapper around the system `cal` command on Unix-like systems.

Without arguments, it displays:

- The previous month
- The current month
- The next month

When arguments are supplied, they are passed to the system `cal` command.


### check_reboot.sh

Checks whether the system should be rebooted after package updates.

It primarily uses two signals:

- `/var/run/reboot-required`
- `needrestart`

This combines the Debian/Ubuntu package mechanism with inspection of running processes that still use old binaries or libraries.


### check_sshd_config.sh

Checks SSH daemon and TCP Wrappers-related settings on Linux and macOS.

It reports major sshd settings such as Port, PermitRootLogin, and PasswordAuthentication.


### get_resources.sh

Collects and reports system resources and major system state on Linux and macOS.

The report includes:

- OS / distribution
- CPU
- Memory
- Disk
- Processes
- Power / thermal status
- Network
- Time synchronization
- DNS
- Security-related logs
- fail2ban


### restart-sshd.sh

Provides an operational utility for restarting the SSH daemon while handling SSH daemon configuration.


### server_alive_check.sh

Monitors the existence and freshness of `_is_alive` files received from multiple servers.

Hosts whose files are older than the configured threshold are treated as alert candidates.

Hosts with the `VM` prefix are treated as virtual hosts and are normally excluded from alerts.


### userlist.py

Retrieves and displays system users.


### usershells.py

Extracts and displays users with login-capable shells.

It is also used by `installer/setup_aliases.sh`.


### vacuum-safari.sh

Performs maintenance on Safari-related databases and data.


### fluent-start.sh

Provides a wrapper or startup utility for Fluentd.


### hadoop-start.sh

Provides a startup utility for Hadoop-related processes.


### vmplayer-start.sh

Provides a utility for starting VMware Player.


### waitlock.rb

Uses a lock to make multiple processes or jobs wait rather than run concurrently.


## 17. Other Utilities

### namecalc.py

Performs numerology calculations by converting input strings into numeric values.


### namecalc.rb

Provides the same class of numerology calculation in Ruby.


### simple_passwd.py

Generates simple passwords in Python.


### simple_passwd.rb

Provides the same class of password generation in Ruby.


## 18. Role of installer/

`installer/` contains some of the most state-changing functionality in this repository.

Many top-level utilities primarily process input and return output. Installer scripts, by contrast, may perform operations such as:

- Installing or uninstalling packages
- Modifying `/etc`
- Creating systemd units or drop-ins
- Deploying cron jobs
- Configuring logrotate
- Creating user accounts
- Changing filesystem permissions or ownership
- Changing kernel parameters
- Modifying desktop environments
- Downloading, building, and installing software from source

The current `installer/` directory contains 80 scripts.

### Installer capability index

| Area | Main entry points | Scope | Main effects |
| --- | --- | --- | --- |
| Debian full setup | `debian_init.sh`, `debian_env.sh`, `debian_apt.sh`, `debian_setup.sh` | Debian / Ubuntu based systems | Packages, shells, dotfiles, monitoring, security configuration, and sysctl |
| Debian desktop setup | `debian_desktop_apt.sh`, `debian_desktop_setup.sh`, `debian_xfce_setup.sh`, `debian_gnome_flashback_setup.sh`, `debian_gnome_setup.sh` | Debian desktop environments | Desktop packages, workspaces, keybindings, appearance, and desktop services |
| macOS full setup | `macos_setup.sh`, `create_emergencyadmin.sh`, `install_brews.sh`, `reinstall_brew.sh`, `macos_finder_settings.sh`, `set_ipv6_macos.sh` | macOS | Dotfiles, Homebrew, FileVault recovery user, Finder settings, and network configuration |
| Dotfiles and editors | `install_dotfiles.sh`, `install_dotvim.sh`, `setup_dot_ipython.sh`, `setup_nvim.sh`, `setup_jupyter_themes.sh`, `setup_xdg_dirs_en.sh` | User environments | Deploys editor, shell, IPython, Jupyter, and desktop user configuration |
| Languages and runtimes | `install_R_libs.sh`, `install_python.sh`, `install_pip.sh`, `install_conda.sh`, `install_ruby.sh`, `install_gems.sh`, `install_zsh.sh`, `install_mecab-stack.sh` | Primarily Unix-like systems | Downloads, builds, installs, and configures language runtimes and libraries |
| Monitoring and scheduled operations | `install_apache_log_analysis.sh`, `install_chkrootkit.sh`, `install_clamscan.sh`, `install_get_resources.sh`, `install_munin.sh`, `install_rsync_backup.sh`, `install_run_tests.sh` | Primarily Linux / Debian | Deploys executables, configuration, cron jobs, logrotate, and monitoring services |
| System and security configuration | `configure_sysctl.sh`, `setup_iptables.sh`, `setup_pamd.sh`, `setup_securetty.sh`, `setup_dos_guard.sh`, `setup_apache2_ssl.sh`, `setup_crontab.sh`, `purge_kernels.sh`, `remove-tracker.sh` | Primarily Linux / Debian | Changes `/etc`, kernel or security policy, systemd configuration, services, and installed packages |
| Chrome and GDM | `install_google_chrome.sh`, `install_gdm_themes.sh`, `install_gdm_themes2.sh` | Debian / Linux desktop | Configures the Chrome APT source or installs display-manager themes |


## 19. Debian Full Setup

### debian_init.sh

Acts as the top-level orchestrator for initial setup of a Debian-based machine.

The main workflow is:

    debian_env.sh
        ↓
    debian_apt.sh
        ↓
    debian_setup.sh
        ↓
    optional desktop setup

Desktop options include:

    --xfce
    --gnome-flashback
    --gnome


### debian_env.sh

Configures the basic system environment for Debian-based systems.

Its main operations are:

- `ja_JP.UTF-8` locale setup
- APT update / upgrade / cleanup
- admin / wheel group creation
- ext filesystem tuning


### debian_apt.sh

Installs a broad set of standard and development packages on Debian-based systems.

Major categories include:

- Base tools
- Editors
- Archivers
- Build / debug toolchains
- System administration
- Networking
- Security / antivirus
- Debian packaging
- TeX
- Image / EXIF tools
- Programming languages
- Development libraries
- Source control
- Databases


### debian_setup.sh

Performs system configuration after package installation.

Its main operations include:

- Changing the login shell for the user and root to Zsh
- Deploying base dotfiles
- Cloning, updating, and installing dot_zsh
- Installing dot_vim
- Installing dot_emacs when conditions are met
- Redeploying sysadmin scripts
- Setting up a consistent Python command symlink
- Setting up resource reporting
- Installing chkrootkit
- Installing ClamAV
- Installing Munin
- Configuring iptables
- Configuring PAM
- Configuring securetty
- Configuring rsyslog
- Configuring memcached
- Applying the NTP restart policy
- Installing system crontab entries
- Configuring mail aliases
- Configuring MOTD
- Installing IPython dotfiles
- Normalizing ownership of `/usr/src` and `/usr/local/src`
- Applying sysctl settings
- Cleaning up `.bash_history`


## 20. Debian Desktop Setup

### debian_desktop_apt.sh

Installs desktop-oriented packages on Debian-based systems.

Targets include:

- Xfce
- GNOME themes
- Fcitx5 / Mozc
- Japanese fonts
- GUI package tools
- Multimedia components
- Icon / cursor themes
- Desktop utilities
- Browsers
- Mail clients
- Office suites
- Media applications
- Document readers
- Remote-access tools


### debian_desktop_setup.sh

Acts as a dispatcher for desktop-environment-specific setup scripts.

    --xfce
        debian_xfce_setup.sh

    --gnome-flashback
        debian_gnome_flashback_setup.sh

    --gnome
        debian_gnome_setup.sh

It does not install desktop packages itself.


### debian_xfce_setup.sh

Configures the Xfce user environment.

Its main targets include:

- Automount behavior
- Desktop icons
- Nine workspaces
- Workspace switching shortcuts
- Window movement shortcuts
- Maximize shortcut
- Screenshots
- Screen locking
- Application launchers
- Automatic locking / blanking
- Dark theme
- Panel configuration
- Wallpaper
- Power management
- xfce4-terminal profile
- xmodmap autostart
- Keyboard repeat


### debian_gnome_flashback_setup.sh

Configures a GNOME Flashback / Metacity session.

Its main targets include:

- Media handling
- Desktop icons
- Nine workspaces
- Window-manager keybindings
- Application shortcuts
- Manual lock behavior
- Automatic lock / idle behavior
- Dark theme
- Keyboard repeat
- Terminal profile
- xmodmap autostart
- Optional gnome-panel reset


### debian_gnome_setup.sh

Configures a GNOME Shell session.

Its main targets include:

- Media handling
- Fixed workspaces
- Window-manager keybindings
- Custom application shortcuts
- Lock / idle behavior
- Dark color scheme
- Keyboard repeat
- Terminal profile
- Masking background services such as Tracker, GOA, Evolution, and Rygel


## 21. macOS Full Setup

### macos_setup.sh

Performs grouped setup of the macOS user and development environment.

Its main operations include:

- Base dotfiles
- dot_zsh
- dot_vim
- dot_emacs
- Sysadmin scripts
- emergencyadmin
- IPython dotfiles
- Finder settings
- System folder localization
- Zsh compinit permission repair
- Ownership normalization for `/opt/python`, `/opt/ruby`, and `/usr/local/src`
- `.bash_history` cleanup


### create_emergencyadmin.sh

Creates a local macOS administrator account named `emergencyadmin`.

It grants SecureToken and makes the account FileVault-enabled, providing a backup unlock path for encrypted systems.


### install_brews.sh

Uses Homebrew to install a broad set of packages for development, text processing, system administration, and related tasks.

It includes GNU coreutils and similar tools to reduce behavioral differences between macOS and GNU/Linux command environments.


### reinstall_brew.sh

Completely removes and reinstalls Homebrew.

After reinstalling Homebrew, it also performs:

- Bulk package installation
- compinit repair


### macos_finder_settings.sh

Changes Finder and screenshot preferences.

Major changes include:

- Disabling screenshot shadows
- Showing hidden files in Finder
- Changing the default screenshot file name
- Suppressing `.DS_Store` creation on network shares


### macos_system_folder_localizations.sh

Creates or removes `.localized` files in macOS system directories in order to enable or disable folder localization.


### set_ipv6_macos.sh

Enables or disables IPv6 across detected macOS network services.

It checks the current state and changes only settings that require modification.


### setup_karabiner-elements.sh

Deploys and configures Karabiner-Elements settings.


### vmware-rebuild-and-sign.sh

Assists with rebuilding and code-signing VMware-related components on macOS.


## 22. Dotfiles, Editors, and User Environment Installers

### install_dotfiles.sh

Deploys predefined dotfiles from the repository into user environments.

Targets include:

- Zsh
- Vim
- Git
- Emacs
- SSH
- GNU Screen
- IPython
- Other user configuration

It accounts for platform differences such as macOS and Linux and also normalizes required directories and permissions.


### install_dotvim.sh

Deploys dot_vim configuration into `.vim`.

It supports a custom installation path.

If `~/.config/nvim` exists, it also deploys the same configuration there for NeoVim compatibility.

An uninstall mode is provided.


### setup_dot_ipython.sh

Deploys IPython user configuration.


### setup_nvim.sh

Configures the NeoVim user environment.


### setup_jupyter_themes.sh

Configures Jupyter themes and display settings.


### setup_xdg_dirs_en.sh

Configures XDG user-directory names to use English names.


## 23. Language, Runtime, and Build Environment Installers

### install_R_libs.sh

Uses `etc/install_mylibs.R` to install a predefined set of R libraries in bulk.


### install_python.sh

Downloads, builds, and installs Python from the official source.

It accepts a version and installation prefix.

By default, it uses a version-series directory under `/opt/python/x.y`.

It also supports user-local installation through `--no-sudo`.


### install_pip.sh

Uses pip to install a predefined set of Python libraries for data analysis, machine learning, scientific computing, web development, and related tasks.


### install_conda.sh

Installs a broad set of libraries and tools into a Conda environment for data analysis, machine learning, web development, and related tasks.


### setup_python_symlink.sh

Creates or normalizes Python command symlinks in order to provide a consistent command path.


### install_ruby.sh

Downloads, builds, and installs Ruby from the official source.

It supports a version, installation prefix, and `--no-sudo`.

By default, it uses a directory under `/opt/ruby/x.y`.


### install_gems.sh

Updates RubyGems and installs a predefined set of gems for web development, data processing, and related tasks.


### create_ubygems.sh

Creates:

    ubygems.rb

in Ruby's standard library directory with:

    require 'rubygems'

This is provided for compatibility with older tools such as older Vim plugins.


### install_zsh.sh

Downloads, builds, and installs Zsh from source.

It supports a version, installation prefix, and `--no-sudo`.


### install_autoconf.sh

Downloads, builds, and installs GNU Autoconf from source.

It supports version selection and optional preservation of source files.


### install_ncurses.sh

Downloads, builds, and installs ncurses from source into a selected prefix.

It supports version selection, a custom prefix, and `--no-sudo`.


### install_paco.sh

Downloads, builds, and installs paco from source.


### install_talib.sh

Downloads, builds, and installs TA-Lib from source.


### install_cassandra.sh

Downloads and installs Apache Cassandra and configures required directories and permissions.

It supports version selection.


### install_resin.sh

Downloads, builds, and installs the Resin application server.


### install_des.sh

Downloads, compiles, and installs DES software.

Source preservation can be enabled or disabled.


### install_mecab-stack.sh

Builds a Japanese text-processing stack under a single installation prefix.

It installs:

- MeCab
- mecab-ipadic-NEologd
- CaboCha

The default prefix is:

    /opt/mecab-stack


### uninstall_mecab_local.sh

Removes locally installed MeCab-related components.


### install_truecrypt.sh

Downloads, installs, and configures TrueCrypt 7 for Linux.


### install_veracrypt.sh

Installs and configures VeraCrypt for Linux according to system architecture.


## 24. Monitoring and Cron Deployment Installers

### install_apache_log_analysis.sh

Deploys the Apache log-analysis environment into the system.

It primarily installs and configures:

- apache_log_analysis.sh
- apache_calculater.py
- apache_blog_analysis.py
- Configuration files
- Cron jobs
- Permissions


### install_awstats.sh

Installs AWStats through APT and configures it for Apache log analysis.

It also handles Apache configuration, log permissions, service restart, and statistics updates.


### install_chkrootkit.sh

Deploys periodic chkrootkit scanning.

It configures cron, log rotation, and the required directories and logs.

On Debian-style environments, it can initialize an expected baseline from the first scan.


### install_clamscan.sh

Deploys periodic ClamAV scanning.

It primarily installs:

    /etc/cron.exec/clamscan.sh
    /etc/cron.config/clamscan.conf
    /etc/cron.d/clamscan

along with logrotate configuration.

An uninstall mode is provided.


### disable_freshclam_syslog.sh

Creates a systemd drop-in for `clamav-freshclam` that redirects stdout and stderr to `/dev/null`.

It restarts the affected systemd service so the setting takes effect.


### install_fix-permissions.sh

Deploys a periodic permission-repair job.

It configures the cron job, log directory, and logrotate.


### install_get_resources.sh

Deploys `get_resources.sh` as an automated server-resource reporting job.

It also configures cron and log rotation.


### install_munin.sh

Installs and configures Munin and Munin-node on Debian-based systems and restarts the required services.


### munin_plugins_links.sh

Uses commands generated by `munin-node-configure` to configure Munin plugin symlinks.


### install_munin-symlink.sh

Deploys the Munin symlink monitor into:

    /etc/cron.exec
    /etc/cron.config

and configures periodic cron execution.


### install_munin-sync.sh

Deploys scripts, configuration, and cron jobs for Munin synchronization.

Its uninstall mode removes the related files.


### install_rsync_backup.sh

Deploys the rsync-based removable-disk backup system.

It primarily installs:

    /etc/cron.exec/rsync_backup.sh
    /etc/cron.hourly/rsync_backup
    /etc/cron.config/rsync_backup.conf
    /etc/logrotate.d/...

for scheduled operation.


### install_run_tests.sh

Deploys the cron environment used to run repository tests automatically.

It manages the script, configuration, cron job, and logrotate definition.


## 25. System and Security Configuration Installers

### configure_sysctl.sh

Deploys persistent kernel parameters under `/etc/sysctl.d/` on GNU/Linux.

Its main purposes are:

- Optional IPv6 disabling
- IPv4 network-security hardening

It emits only keys that exist on the running kernel.

After writing the settings, it runs `sysctl --system` and verifies a subset of the result.


### setup_iptables.sh

Deploys and applies iptables rules maintained in the repository.


### setup_pamd.sh

Adjusts the `pam_wheel.so` policy in `/etc/pam.d/su`.

It enables the intended:

    auth required pam_wheel.so

and disables:

    auth sufficient pam_wheel.so trust

to prevent passwordless root access through wheel-group membership.


### setup_securetty.sh

Clears `/etc/securetty` when it exists as a regular file.

This relaxes the TTY restriction on root login and is therefore a security-sensitive operation.


### setup_dos_guard.sh

Deploys DoS protection for Apache2 using:

- mod_evasive
- fail2ban

It deploys configuration files into system locations, creates required directories, and reloads Apache and fail2ban.


### setup_aliases.sh

Configures `/etc/aliases` so system mail for login-capable users is centralized to root.

It removes an unnecessary alias for the currently executing user, applies the result using `newaliases`, and also cleans existing local mail spools.


### setup_crontab.sh

Ensures that the system-wide `/etc/crontab` contains schedules for running:

    /etc/cron.weekday
    /etc/cron.weekend

through `run-parts`.

The required directories are created when missing.


### setup_aptconf.sh

Deploys the repository's APT configuration into the system and leaves it available for manual editing as required.


### setup_apache2_ssl.sh

Configures an SSL-enabled Apache2 environment.

It handles:

- SSL certificate generation
- Virtual-host template deployment
- Host-FQDN-based configuration naming
- SSL module enablement
- Site enablement

The deployed host-specific configuration is intended to be reviewed and edited for each target host.


### setup_memcached_conf.sh

Deploys and configures the memcached daemon configuration.


### setup_ntp_restart_policy.sh

Deploys a systemd drop-in for available NTP services.

Candidate services are:

    ntpsec.service
    ntp.service
    myntp.service

The policy is:

    Restart=on-failure
    RestartSec=30

When changes are made, `systemctl daemon-reload` is run.

The services themselves are not restarted automatically.


### setup_motd.sh

Configures the system MOTD.


### setup_rsyslog_cron.sh

Deploys rsyslog configuration for cron logs.


### setup_rsyslog_logrotate.sh

Deploys logrotate configuration for rsyslog and related logs.


### setup_rsyslog_postfix.sh

Deploys rsyslog configuration for Postfix logs.


### setup_sysadmin_scripts.sh

Installs or uninstalls the system-administration scripts into their designated system locations.


### setup_tune2fs.sh

Adjusts ext filesystem parameters and maintenance settings.


### purge_apt_cache.sh

Purges residual APT configuration left behind by removed packages.

This is a destructive maintenance operation because it permanently deletes residual configuration files.


### purge_kernels.sh

Removes old kernel packages on Ubuntu-based systems.

The currently running kernel is excluded from removal.


### remove-tracker.sh

Stops, disables, and removes the Tracker indexing service on Debian-based GNOME environments.

Because this affects GNOME search functionality, it should be treated as an explicit system customization.


## 26. Chrome, GDM, and Related Installers

### install_google_chrome.sh

Installs Google Chrome Stable on Debian and configures it for management through the official APT repository.

It compares the signing-key fingerprint and refreshes the keyring when key rotation or corruption is detected.

The script intentionally does not run `apt update` itself.


### install_gdm_themes.sh

Downloads, extracts, and installs a GDM theme archive into the system.


### install_gdm_themes2.sh

Downloads, extracts, and installs a separate GDM Themes 2 series.


## 27. Automated Operations in cron/bin

`cron/bin/` contains executables intended for unattended operation rather than merely copies of manual utilities.

The current jobs include the following.

| Job | Purpose | Operational role |
| --- | --- | --- |
| `cron/bin/apache_log_analysis` | Run Apache log analysis | Cron entry point |
| `cron/bin/chkrootkit` | Run periodic rootkit scanning | Scheduled security check |
| `cron/bin/clamscan` | Start the ClamAV scan workflow | Cron entry point |
| `cron/bin/clamscan.sh` | Perform the main ClamAV scan logic | Main scan implementation |
| `cron/bin/fix-permissions.sh` | Repair configured filesystem permissions | Periodic maintenance |
| `cron/bin/get_resources` | Generate system-resource reports | Periodic reporting |
| `cron/bin/munin-symlink.sh` | Monitor Munin-related symlink state | Monitoring |
| `cron/bin/munin-sync.sh` | Synchronize Munin data or state | Synchronization |
| `cron/bin/rsync_backup` | Start the rsync backup workflow | Cron trigger |
| `cron/bin/rsync_backup.sh` | Perform removable-disk backup and synchronization | Main backup implementation |
| `cron/bin/run_tests` | Run interpreter-matrix tests and repository-wide checks | Scheduled quality gate |


### cron/bin/apache_log_analysis

Runs Apache log analysis from cron.


### cron/bin/chkrootkit

Runs periodic chkrootkit scanning.


### cron/bin/clamscan

Acts as the cron entry point for the ClamAV scan job.


### cron/bin/clamscan.sh

Implements the main ClamAV scan logic.


### cron/bin/fix-permissions.sh

Periodically repairs configured filesystem permissions.


### cron/bin/get_resources

Periodically generates system-resource reports.


### cron/bin/munin-symlink.sh

Monitors Munin-related symlink state.


### cron/bin/munin-sync.sh

Synchronizes Munin data or state.


### cron/bin/rsync_backup

Acts as the cron trigger for rsync backup.


### cron/bin/rsync_backup.sh

Implements the main removable-disk backup and synchronization logic.

It handles device health, timestamp updates, unnecessary metadata cleanup, and local or SSH-based rsync operations.

Backup data is separated into capacity tiers:

    base
    extended


### cron/bin/run_tests

Runs tests across Python and Ruby versions and performs repository-wide quality gates.


## 28. Cron Configuration

`cron/etc/` contains the configuration and scheduling assets used by cron jobs.

Configuration files:

    clamscan.conf
    fix-permissions.conf
    munin-symlink.conf
    munin-sync.conf
    rsync_backup.conf
    run_tests.conf

cron.d:

    cron.d/clamscan
    cron.d/reboot
    cron.d/shutdown

logrotate:

    logrotate.d/apache2
    logrotate.d/apache_summary
    logrotate.d/chkrootkit
    logrotate.d/clamscan
    logrotate.d/fix-permissions
    logrotate.d/resources
    logrotate.d/rsync_backup
    logrotate.d/run_tests

Installers deploy these files into the corresponding `/etc` locations.


## 29. Support Configuration under etc/

`etc/` contains data files, configuration files, and templates used by executables.

Major areas include the following.

Japanese input:

    aa.txt

Apache:

    apache/mods-available/evasive.conf
    apache/sites-available/hostname.sitename.conf
    apache/sites-available/hostname.sitename-ssl.conf
    apache/snippets/acme-rewrite-exception.conf
    apache_ignore.list

APT:

    apt.conf
    apt.conf.d/...

Dashcam:

    dashcam_sync.conf

fail2ban:

    fail2ban/filter.d/apache-evasive.conf
    fail2ban/jail.local

Fluentd:

    fluentd/conf/fluent-automaticruby-mongodb.conf

Git:

    git-archive-repo.conf

GNOME:

    gnome/gnome-shortcuts.conf
    gnome/gnome-wm-keys.conf

GPX:

    gpx_sync.conf

Instagram:

    insta_sync.conf
    insta_update.conf

iptables:

    iptables/rules.v4

Munin:

    munin-apache.conf

R:

    install_mylibs.R

rsyslog:

    rsyslog.d/10-cron.conf
    rsyslog.d/30-postfix.conf

SD card extraction:

    sd_extract.conf

File transfer:

    send_files.conf

SSH:

    sshd_config.d/000-sshdconfig.conf

sudo:

    sudoers
    sudoers_macos

Xfce:

    xfce/terminalrc


## 30. dot_files/

`dot_files/` contains configuration assets deployed into user environments by `install_dotfiles.sh` and related installers.

Major targets include:

- R
- Conda
- Fcitx5
- Emacs
- RubyGems
- Git
- Vim / GVim
- NeoVim
- IPython
- Karabiner-Elements
- Matplotlib
- GNU Screen
- SSH
- X input
- Xmodmap
- Zsh

This directory also contains bundled third-party trees such as Vim plugins.

`FEATURES.md` describes what user environment is provided, but it does not redefine every bundled third-party plugin as a feature of the `scripts` repository itself.


## 31. Destructive or System-Wide Features

This repository contains many commands that modify system state in addition to read-only utilities.

The target and the script's own header documentation should be reviewed before running operations in the following categories.

| Operation type | Representative examples | Main risk |
| --- | --- | --- |
| File deletion | `cleanup-junk-files.sh`, `cltmp.sh`, `remove-repo.sh`, `purge_apt_cache.sh`, `purge_kernels.sh` | Permanently removes files, repositories, package state, or old kernels |
| Destructive Git operations | `git-all-pull.sh --hard`, `git-all-pull.sh --delete-remote-branches`, `git-create-repo.sh --delete`, `remove-repo.sh -x` | Discards local changes or deletes repositories or remote branches |
| Permission and ownership changes | `chmodtree.py`, `fix_compinit.sh`, `install_dotfiles.sh`, `setup_sysadmin_scripts.sh` | Changes filesystem metadata and access behavior |
| System configuration | `configure_sysctl.sh`, `setup_iptables.sh`, `setup_pamd.sh`, `setup_securetty.sh`, `setup_dos_guard.sh`, `setup_apache2_ssl.sh`, `setup_crontab.sh` | Changes kernel, network, authentication, web-server, or scheduled-job configuration |
| Package installation or removal | `installer/debian_apt.sh`, `installer/install_*.sh`, `installer/purge_*.sh`, `installer/remove-tracker.sh` | Installs or removes system software and may alter services |


### File deletion

    cleanup-junk-files.sh
    cltmp.sh
    clear_chromium_cache.sh
    remove-repo.sh
    purge_apt_cache.sh
    purge_kernels.sh
    remove-tracker.sh


### Destructive Git operations

    git-all-pull.sh --hard
    git-all-pull.sh --delete-remote-branches
    git-create-repo.sh --delete
    remove-repo.sh -x


### Permission / ownership changes

    chmodtree.py
    fix_compinit.sh
    unfix_compinit.sh
    install_dotfiles.sh
    setup_sysadmin_scripts.sh


### System configuration

    configure_sysctl.sh
    setup_iptables.sh
    setup_pamd.sh
    setup_securetty.sh
    setup_dos_guard.sh
    setup_apache2_ssl.sh
    setup_crontab.sh


### Package installation / removal

    installer/debian_apt.sh
    installer/debian_desktop_apt.sh
    installer/install_*.sh
    installer/purge_*.sh
    installer/remove-tracker.sh


## 32. root and sudo

Many top-level utilities can be used as a regular user.

Installers, however, frequently require sudo or root privileges because they modify areas such as:

- `/etc`
- `/usr/local`
- `/opt`
- `/usr/local/src`
- systemd
- cron
- Package managers
- User accounts
- Filesystem ownership

Some installers, such as:

    install_python.sh
    install_ruby.sh
    install_zsh.sh
    install_ncurses.sh

support `--no-sudo` together with a custom prefix, making user-local installation possible.

The exact privilege requirement for each script is defined by that script's own header documentation.


## 33. OS-Specific Behavior

This repository supports multiple operating systems, but not every script runs on every platform.


### Debian / Ubuntu based systems

Representative examples:

    apt-upgrade.sh
    debian_init.sh
    debian_env.sh
    debian_apt.sh
    debian_setup.sh
    debian_desktop_apt.sh
    purge_apt_cache.sh
    purge_kernels.sh
    setup_aptconf.sh


### macOS

Representative examples:

    brew-upgrade.sh
    du.py
    fix_compinit.sh
    unfix_compinit.sh
    create_emergencyadmin.sh
    install_brews.sh
    reinstall_brew.sh
    macos_finder_settings.sh
    macos_setup.sh
    macos_system_folder_localizations.sh
    set_ipv6_macos.sh


### Linux

Representative examples:

    configure_sysctl.sh
    get-device.sh
    get-mountpoint.sh
    get-serial.sh
    install_clamscan.sh
    install_chkrootkit.sh
    setup_iptables.sh


### Multiple Unix-like platforms

Many top-level text, file, and Git utilities can be used on both Linux and macOS.

The exact support conditions are defined by each script's own header documentation.


## 34. Configuration File Handling

Features that require site-specific values read external configuration rather than embedding hostnames, paths, credentials, or similar values directly in the script.

Representative examples include:

    dashcam_sync.sh
        etc/dashcam_sync.conf

    git-archive-repo.sh
        etc/git-archive-repo.conf

    gpx_sync.sh
        etc/gpx_sync.conf

    sd_extract.sh
        etc/sd_extract.conf

    send_files.sh
        etc/send_files.conf

After cron deployment, many configurations are installed under:

    /etc/cron.config/

Separating code from site-specific configuration is a core operational model of this repository.


## 35. Help and Header Documentation

The exact interface of each executable is documented in the header documentation at the top of its source file.

Many current scripts can display that header information through:

    -h
    --help

and:

    -v
    --version

However, the exact option set differs by script.

`FEATURES.md` intentionally does not duplicate every option.

For example, `FEATURES.md` tells users what `git-all-pull.sh` can do, but the exact current option set, destructive behavior, and usage are defined by:

    git-all-pull.sh --help

and the source header.


## 36. Finding a Feature by Purpose

When looking for a command by purpose, the following mapping is generally useful.


Format writing or HTML:

    add_hr_h2.py
    fix_anchor.py
    format_html.py
    aozora_prepare.rb


Inspect files or directories:

    dirsize.py
    du.py
    els.py
    find_range.py
    list_file_counts.py


Modify file trees:

    chmodtree.py
    flatdirs.py
    cleanup-junk-files.sh
    swapext.py


Manage Git repositories:

    git-all-pull.sh
    git-archive-repo.sh
    git-co-remote-branch.sh
    git-create-repo.sh
    git-follow-origin.sh
    git-symlink.sh
    remove-repo.sh


Inspect disks and mounts:

    get-device.sh
    get-mountpoint.sh
    get-serial.sh


Inspect system state:

    get_resources.sh
    check_reboot.sh
    check_sshd_config.sh
    server_alive_check.sh


Perform backups or synchronization:

    dashcam_sync.sh
    gpx_sync.sh
    sd_extract.sh
    cron/bin/rsync_backup.sh


Maintain Python code:

    pyck.py
    find_pycompat.py
    check_header_doc.py
    run_tests.sh


Provision a Debian machine:

    installer/debian_init.sh


Provision a macOS machine:

    installer/macos_setup.sh


Build individual software from source:

    installer/install_python.sh
    installer/install_ruby.sh
    installer/install_zsh.sh
    installer/install_ncurses.sh
    installer/install_autoconf.sh
    installer/install_talib.sh


Deploy monitoring or periodic operations:

    installer/install_chkrootkit.sh
    installer/install_clamscan.sh
    installer/install_get_resources.sh
    installer/install_munin.sh
    installer/install_munin-symlink.sh
    installer/install_munin-sync.sh
    installer/install_rsync_backup.sh
    installer/install_run_tests.sh


## 37. Scope of This Document

This `FEATURES.md` answers the following question:

"What user-facing capabilities exist in the scripts repository?"

This document covers:

- All top-level executables
- All setup and installation scripts under `installer/`
- Automated jobs under `cron/bin/`
- Support configuration under `cron/etc/`
- Major configuration and templates under `etc/`
- The user environment provided by `dot_files/`

It does not duplicate:

- The full Version History of every script
- Every option word-for-word
- Every test case
- Internal APIs of bundled third-party source
- General manuals for standard commands

The final source of truth for the interface of each file is the file's own header documentation and implementation.

The purpose of this document is to let users understand, before opening roughly one hundred top-level scripts and many installers one by one:

"What already exists?"
"Which command should I inspect?"
"Is that command read-only, or does it change system state?"
