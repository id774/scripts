# Scripts Collection Feature Reference

This document serves as a high-level index of the features available in the `scripts` repository. Its purpose is to describe what each feature provides and help users locate the relevant scripts.

**This document must not be used to record detailed designs, implementation details, or script-specific operational instructions.** Such information belongs in the relevant script's header, source code, or dedicated documentation. Feature descriptions must remain concise and limited to the information necessary to understand each feature's purpose.

The repository contains independent utilities, installers, scheduled jobs, and supporting configuration. The README covers repository-wide setup and layout. Individual executable headers define their usage, options, and requirements. This index describes capabilities and important operational boundaries without duplicating those interfaces.

## 1. Repository Feature Structure

| Area | Role |
| --- | --- |
| Repository root | Utilities for direct use |
| `installer/` | Installers and environment configuration |
| `cron/bin/` | Scheduled operational jobs |
| `cron/etc/` | Configuration for scheduled jobs |
| `etc/` | Shared configuration and templates |
| `dot_files/` | User-environment settings |
| `test/` | Repository validation |
| `doc/` | Policies, releases, and feature references |

## 2. Top-Level Scripts

Top-level executables are grouped below by their primary purpose. Use the script header for invocation details.

### Top-level capability index

| Area | Representative commands | Main purpose |
| --- | --- | --- |
| HTML, writing, and content transformation | `format_html.py`, `fix_anchor.py`, `aozora_prepare.rb` | Prepare and transform documents |
| Japanese input and dictionaries | `anthy_create_aa_dic.sh`, `convert_msime2canna.rb` | Create and convert dictionaries |
| Apache and WordPress | `apache_log_analysis.sh`, `apache_blog_analysis.py`, `wp_cachectl.py` | Analyze web traffic and manage caches |
| Package management | `apt-upgrade.sh`, `brew-upgrade.sh`, `dpkg-hold.sh` | Maintain packages |
| Files and filesystems | `chmodtree.py`, `dirsize.py`, `find_range.py`, `flatdirs.py` | Inspect or modify files |
| Zsh history and environment | `erase_history.py`, `filter_history.sh`, `fix_compinit.sh` | Maintain shell data and settings |
| Storage and encrypted volumes | `get-device.sh`, `luksmount.py`, `tcmount.py` | Inspect devices and access encrypted volumes |
| Git repositories | `git-all-pull.sh`, `git-archive-repo.sh`, `remove-repo.sh` | Synchronize, archive, or remove repositories |
| Networking and transfers | `pyping.py`, `wakeonlan.py`, `send_files.sh` | Check reachability and transfer data |
| Dashcam, GPX, and Instagram | `dashcam_sync.sh`, `gpx_sync.sh`, `insta_sync.sh` | Synchronize media and location data |
| Development and testing | `pyck.py`, `check_header_doc.py`, `run_tests.sh` | Validate code and repository conventions |
| System administration | `get_resources.sh`, `check_reboot.sh`, `server_alive_check.sh` | Inspect system state and availability |
| Other utilities | `namecalc.py`, `simple_passwd.py` | Small standalone tools |

## 3. HTML, Writing, and Content Transformation

Utilities for preparing, formatting, and converting text, HTML, and documents.

### add_hr_h2.py

Inserts separators before HTML headings, avoiding duplicate separators. Can write a separate file or update the input.

### aozora_prepare.rb

Prepares Aozora Bunko text by normalizing encoding, annotations, spacing, and line breaks.

### fix_anchor.py

Normalizes reference-link placement around Japanese punctuation in HTML and Markdown.

### format_html.py

Formats HTML while preserving the contents of code and preformatted elements. It is a formatter, not an HTML repair tool.

### html2yaml.py

Converts HTML content into a YAML-compatible representation.

### remove_space_eol.sh

Removes trailing whitespace from lines.

### zero_padding.py

Adds zero padding to numeric strings and filenames.

### platex2pdf.sh

Provides a workflow for generating PDF documents with pLaTeX.

## 4. Japanese Input and Dictionary Conversion

Tools for preparing and converting Japanese input dictionaries.

### anthy_create_aa_dic.sh

Creates an Anthy dictionary for ASCII art entries from MS-IME dictionary data.

### convert_msime2canna.rb

Converts MS-IME dictionary entries, including emoticons, into Canna format.

## 5. Apache, Web Servers, and WordPress

Utilities for web access analysis and WordPress operations.

### apache_log_analysis.sh

Summarizes Apache SSL access logs by URL, referrer, client, and time period. Supports configured IP exclusions. Article-specific view estimates are handled by `apache_blog_analysis.py`.

### apache_calculater.py

Aggregates Apache access counts and estimates client-cache hit proportions by IP address. Accepts multiple logs, including compressed files.

### apache_blog_analysis.py

Estimates WordPress article engagement using candidate page views, asset-confirmed views, and sessions. These are estimates, not exact human visitor counts, because caches and browser behavior affect the evidence.

### wp_cachectl.py

Uses WP-CLI for event-driven cache management following changes or troubleshooting. Covers WordPress transient, object, and page caches. External CDN and browser caches are guidance-only.

## 6. Package Updates and Package Management

Commands for package maintenance and update controls.

### apt-upgrade.sh

Runs a grouped APT package maintenance workflow on Debian-based systems.

### brew-upgrade.sh

Runs a grouped Homebrew package maintenance workflow.

### dpkg-hold.sh

Manages Debian package hold states to prevent selected packages from being upgraded automatically.

## 7. Files, Directories, and Filesystems

Commands for filesystem inspection, conversion, and maintenance. Some commands modify or delete data.

### chmodtree.py

Recursively sets directory-tree permissions and ownership according to selected rules. May change filesystem metadata.

### cleanup-junk-files.sh

Removes common temporary and metadata files from a selected directory tree. Deletes matching files.

### clear_chromium_cache.sh

Removes Chromium's `Web Data` data. Close Chromium before using this destructive utility.

### cltmp.sh

Cleans temporary and cache files on Linux and macOS using target-specific retention periods. It intentionally clears the contents of `$HOME/.cache` regardless of age.

### dirsize.py

Reports the sizes of files directly within a directory without recursively counting subdirectory contents.

### du.py

Provides macOS disk usage reporting with depth and hidden-directory controls.

### els.py

Provides detailed file listings with ownership, size, and multiple timestamps.

### exif_drop.sh

Removes EXIF metadata from supported image files.

### find_range.py

Finds files modified within a time range, with UTC or local-time interpretation.

### flatdirs.py

Collects files from nested directories by moving, copying, or renaming them. Requests confirmation for changes.

### image_resize.py

Resizes images according to selected conditions.

### list_file_counts.py

Counts files in directory trees.

### md5.py

Computes MD5 digests for supplied input or files. MD5 is not suitable for security-sensitive integrity assurance.

### png_info.py

Inspects PNG structure and metadata.

### swapext.py

Renames files by changing their extensions.

### tree.sh

Displays directory hierarchies.

### unixtime2date.py

Converts between Unix timestamps and readable date-time values.

### unzip_subdir.py

Extracts a selected portion of a ZIP archive into a target directory.

## 8. Zsh History

Utilities that edit Zsh command history.

### erase_history.py

Removes a selected number of recent entries from Zsh history while handling the utility's own invocation.

### filter_history.sh

Filters Zsh history entries by pattern and creates a backup before updating the history file.

## 9. Zsh and Shell Environment Maintenance

Tools for shell and user-environment configuration.

### fix_compinit.sh

Repairs ownership and permissions of Homebrew Zsh completion directories on macOS to resolve insecure-directory warnings.

### unfix_compinit.sh

Reverts the permission and ownership changes made by `fix_compinit.sh`.

### xmap.sh

Applies the user's Xmodmap configuration when the required command and file are available.

## 10. Block Devices, Mounts, and Encrypted Volumes

Storage utilities for locating devices and managing encrypted volumes. Verify the selected device before state-changing operations.

### get-device.sh

Resolves a mountpoint to its underlying block device, including paths through partitions or device-mapper layers.

### get-mountpoint.sh

Finds the current mountpoint associated with a block device.

### get-serial.sh

Retrieves the hardware serial number of a selected block device.

### luksmount.py

Lists block-device information or opens and mounts a user-selected LUKS volume. Requires explicit confirmation of the physical device and does not automatically select devices.

### tcmount.py

Mounts or unmounts TrueCrypt and VeraCrypt volumes, including external containers and selectable mount targets.

### sd_extract.sh

Copies selected files from configured SD-card sources into a destination. Reports copy failures while continuing independent transfers.

## 11. Git Repository Management

Utilities for bulk and individual Git repository operations. Some commands can discard changes or remove repositories.

### git-all-pull.sh

Updates multiple local Git repositories, with optional remote-branch inspection, pruning, and symlink management. Some modes can discard local changes or delete remote branches.

### git-archive-repo.sh

Creates compressed backups of configured GitHub and local Git repositories, excluding GitLab repositories. Existing archives at the configured destinations are removed before replacements are created.

### git-co-remote-branch.sh

Creates local tracking branches from remote-only branches.

### git-create-repo.sh

Creates or removes Git repositories, with configurable location and permissions. Supports a dry-run mode.

### git-follow-origin.sh

Assists in merging changes from another GitHub repository into a local master branch.

### git-symlink.sh

Maintains home-directory symlinks for local repositories. It can remove broken or repository-related symlinks.

### remove-repo.sh

Removes selected local Git repositories and related symlinks. Uses dry-run by default and requires an explicit option for deletion.

## 12. Networking, Transfers, and Remote Operations

Network utilities for reachability, downloading, and transferring files.

### pyping.py

Performs ping-like reachability tests from Python.

### wakeonlan.py

Sends Wake-on-LAN packets to supported devices.

### wget.py

Downloads resources using a Python-based command.

### wget.rb

Downloads resources using a Ruby-based command.

### send_files.sh

Builds password-protected archives for file delivery, with optional email transmission. Sending content externally requires explicit invocation.

### gpg-import.sh

Assists with importing GPG keys.

## 13. Dashcam, GPX, and Instagram

Utilities for collecting, organizing, and synchronizing media and location data.

### dashcam_sync.sh

Synchronizes configured dashcam recordings to external storage and organizes them by year.

### gpx_sync.sh

Organizes and synchronizes GPX tracks locally and to configured remote hosts.

### insta_downloader.py

Downloads selected Instagram content.

### insta_video_downloader.py

Downloads selected Instagram videos.

### insta_sync.sh

Synchronizes Instagram-related data according to its configuration.

### insta_update.sh

Runs the configured Instagram update workflow.

## 14. Development, Testing, and Repository Maintenance

Tools for validating source code and maintaining the repository.

### check_header_doc.py

Checks executable header comments against repository documentation conventions.

### find_pycompat.py

Checks source-code compatibility with selected Python versions.

### pyck.py

Runs Python linting, formatting, and import cleanup, with inspection and modification modes.

### run_tests.sh

Executes repository Python and Ruby tests using the selected interpreters.

### setup_scripts.sh

Sets the executable permissions expected for scripts in the repository.

### show_version.py

Reports installed Python package versions and importability, distinguishing missing packages from import failures.

## 15. System Administration Utilities

Utilities for inspecting and managing operating-system state.

### cal.py

Displays calendar information through the Unix `cal` command.

### check_reboot.sh

Checks whether a reboot is recommended after updates, using Debian/Ubuntu signals and process-state information.

### check_sshd_config.sh

Reports selected SSH daemon configuration settings on Linux and macOS.

### get_resources.sh

Reports system resources, storage, processes, networking, time synchronization, and selected security state.

### restart-sshd.sh

Restarts the SSH daemon with configuration-handling support.

### server_alive_check.sh

Checks freshness of server liveness markers and reports stale hosts according to its monitoring policy.

### userlist.py

Lists system users.

### usershells.py

Lists users whose shells permit login.

### vacuum-safari.sh

Maintains Safari-related databases and data.

### vmplayer-start.sh

Starts VMware Player.

### waitlock.rb

Coordinates competing jobs using a lock.

## 16. Other Utilities

Small standalone programs.

### namecalc.py

Performs numerology calculations in Python.

### namecalc.rb

Performs numerology calculations in Ruby.

### simple_passwd.py

Generates simple passwords in Python.

### simple_passwd.rb

Generates simple passwords in Ruby.

## 17. Role of installer/

Installers manage operating-system packages, development runtimes, monitoring, and system configuration. Many require elevated privileges and change persistent system state. Use the following capability index to choose the relevant workflow.

### Installer capability index

| Area | Main entry points | Scope |
| --- | --- | --- |
| Debian full setup | `debian_init.sh`, `debian_env.sh`, `debian_apt.sh`, `debian_setup.sh` | Base system and packages |
| Debian desktop | `debian_desktop_apt.sh`, `debian_desktop_setup.sh` | Graphical environment |
| macOS full setup | `macos_setup.sh`, `install_brews.sh`, `reinstall_brew.sh` | macOS environment |
| Dotfiles and editors | `install_dotfiles.sh`, `install_dotvim.sh`, `setup_nvim.sh` | User configuration |
| Languages and runtimes | `install_python.sh`, `install_miniforge.sh`, `install_ruby.sh`, `install_nodejs.sh` | Development runtimes |
| Developer tools | `install_npm.sh`, `install_safe-chain.sh`, `install_vscode.sh` | CLI tools and editors |
| Claude clients | `install_claude_desktop.sh`, `install_claude_code.sh` | Desktop and CLI applications |
| Monitoring and scheduled operations | `install_clamscan.sh`, `install_munin.sh`, `install_rsync_backup.sh` | Periodic jobs |
| System and security configuration | `setup_iptables.sh`, `setup_pamd.sh`, `setup_dos_guard.sh` | Security and system settings |
| Google Chrome | `install_google_chrome.sh` | Browser installation |

## 18. Debian Full Setup

Entry points for provisioning Debian-based systems. These scripts make broad system-level changes.

### debian_init.sh

Coordinates the main Debian setup workflow and optional desktop configuration.

### debian_env.sh

Prepares the base environment, locale, package state, and system defaults.

### debian_apt.sh

Installs the repository's standard and development package collection on Debian-based systems.

### debian_setup.sh

Applies system and user configuration, including shells, dotfiles, monitoring, security services, and scheduled jobs.

## 19. Debian Desktop Setup

Installers and configuration utilities for Debian desktop environments.

### debian_desktop_apt.sh

Installs desktop packages for graphical sessions, Japanese input, fonts, applications, and remote access.

### debian_desktop_setup.sh

Selects and invokes the configuration workflow for Xfce, GNOME Flashback, or GNOME Shell.

### debian_xfce_setup.sh

Configures Xfce workspaces, shortcuts, appearance, input, and power-management behavior.

### debian_gnome_flashback_setup.sh

Configures GNOME Flashback appearance, workspaces, shortcuts, and session behavior.

### debian_gnome_setup.sh

Configures GNOME Shell appearance, workspaces, shortcuts, and related background services.

## 20. macOS Full Setup

Utilities for provisioning and maintaining macOS systems.

### macos_setup.sh

Coordinates macOS user and development environment setup, including dotfiles, system scripts, and selected preferences.

### create_emergencyadmin.sh

Creates an emergency administrator with SecureToken and FileVault access for recovery. Changes macOS account and encryption access state.

### install_brews.sh

Installs the repository's selected Homebrew packages for development and system administration.

### reinstall_brew.sh

Removes and reinstalls Homebrew, then restores a selected package environment. This is a destructive operation.

### macos_finder_settings.sh

Applies Finder, screenshot, and keyboard preferences.

### macos_system_folder_localizations.sh

Manages macOS system folder localization markers.

### set_ipv6_macos.sh

Enables or disables IPv6 on selected macOS network services.

### setup_karabiner-elements.sh

Deploys Karabiner-Elements configuration.

### vmware-rebuild-and-sign.sh

Rebuilds and signs selected VMware components on macOS.

## 21. Dotfiles, Editors, and User Environment Installers

Utilities for deploying and maintaining user configuration files.

### install_dotfiles.sh

Deploys predefined shell, editor, Git, SSH, and application dotfiles. May update permissions and creates a private per-user temporary area.

### install_dotvim.sh

Deploys Vim configuration and supports Neovim integration and removal.

### setup_dot_ipython.sh

Installs IPython user configuration.

### setup_nvim.sh

Configures a Neovim user environment.

### setup_xdg_dirs_en.sh

Configures XDG user directories with English names.

## 22. Language, Runtime, and Build Environment Installers

Installers for programming language runtimes, libraries, and development tools. Installation privileges and supported platforms depend on the script.

### install_R_libs.sh

Installs a predefined collection of R libraries into the selected elevated R environment. Its library target is validated before package installation.

### install_python.sh

Builds and installs version-selected Python under a selected prefix, including user-local installations.

### install_pip.sh

Installs selected Python libraries and Python development tools using pip.

### install_miniforge.sh

Installs version-selected Miniforge with Conda and Mamba on supported Linux systems. Uses a version-series prefix by default and does not initialize the shell.

### install_conda.sh

Installs selected data-science and development packages into the chosen Conda base environment, not the caller's active environment. Miniforge runtime provisioning is handled separately.

### setup_python_symlink.sh

Maintains Python command symlinks for consistent command paths.

### install_ruby.sh

Builds and installs version-selected Ruby under a selected prefix, including user-local installations.

### install_gems.sh

Updates RubyGems and installs selected Ruby development gems.

### create_ubygems.sh

Provides legacy RubyGems compatibility for tools that expect `ubygems.rb`.

### install_nodejs.sh

Installs version-selected Node.js binaries on supported Linux and macOS platforms, including user-local installations.

### install_npm.sh

Updates npm and installs selected global Node.js and TypeScript tools using Aikido Safe Chain. Safe Chain is mandatory, and installations remain in the selected Node.js prefix.

### install_safe-chain.sh

Installs Aikido Safe Chain under a selected prefix on supported Linux and macOS systems. Its runtime directory requires controlled write access and protection of generated CA private-key material.

### install_zsh.sh

Builds and installs a selected Zsh release.

### install_autoconf.sh

Builds and installs selected GNU Autoconf releases.

### install_ncurses.sh

Builds and installs ncurses under a selected prefix, including user-local installations.

### install_talib.sh

Builds and installs TA-Lib from source.

### install_resin.sh

Builds and installs the Resin application server.

### install_mecab-stack.sh

Installs a Japanese text-processing stack containing MeCab, mecab-ipadic-NEologd, and CaboCha.

### uninstall_mecab_local.sh

Removes selected locally installed MeCab components.

### install_truecrypt.sh

Installs and configures TrueCrypt 7 on Linux.

### install_veracrypt.sh

Installs and configures VeraCrypt on supported Linux systems.

### install_vscode.sh

Installs Visual Studio Code Stable through Microsoft's APT repository on Debian/Ubuntu. Supports an opt-in persistent TLS verification bypass that weakens connection authentication.

### install_claude_desktop.sh

Installs Claude Desktop through Anthropic's APT repository on supported Debian/Ubuntu versions. Supports an opt-in persistent TLS verification bypass.

### install_claude_code.sh

Installs Claude Code using APT on supported Debian/Ubuntu systems or Homebrew on macOS. On Debian/Ubuntu, it supports an opt-in persistent TLS verification bypass.

## 23. Monitoring and Cron Deployment Installers

Installers for scheduled monitoring, maintenance, reporting, and backups.

### install_apache_log_analysis.sh

Deploys Apache access-log analysis and scheduled reporting.

### install_chkrootkit.sh

Installs periodic rootkit scanning with associated logs and scheduling.

### install_clamscan.sh

Installs periodic ClamAV scanning and related configuration. Supports uninstallation.

### disable_freshclam_syslog.sh

Suppresses routine syslog output from `clamav-freshclam` through a systemd service override.

### install_fix-permissions.sh

Deploys scheduled filesystem permission maintenance.

### install_get_resources.sh

Deploys scheduled system-resource reporting.

### install_git-pull.sh

Deploys scheduled bulk Git repository updates. Review the installed schedule and execution identity before enabling it.

### install_munin.sh

Installs and configures Munin system monitoring.

### munin_plugins_links.sh

Configures Munin plugin symlinks.

### install_munin-symlink.sh

Deploys scheduled Munin symlink monitoring.

### install_munin-sync.sh

Deploys scheduled Munin synchronization, with an uninstall mode.

### install_rsync_backup.sh

Deploys scheduled removable-disk rsync backups and their supporting configuration.

### install_run_tests.sh

Deploys automated repository test execution.

## 24. System and Security Configuration Installers

Scripts that change system configuration, authentication, services, and installed packages. Inspect each script before applying these changes.

### configure_sysctl.sh

Applies selected persistent Linux kernel settings, including IPv4 security controls and optional IPv6 disabling.

### setup_iptables.sh

Deploys and applies the repository's firewall rules.

### setup_pamd.sh

Configures PAM restrictions on switching to root through wheel-group membership. Alters authentication behavior.

### setup_securetty.sh

Changes root-login TTY restrictions through `/etc/securetty`. This can relax an existing security control.

### setup_dos_guard.sh

Configures Apache denial-of-service protection using mod_evasive and fail2ban.

### setup_aliases.sh

Configures system mail aliases for login-capable users and maintains local mail spools. It empties non-empty mail spool files under `/var/mail`, permanently discarding their contents.

### setup_crontab.sh

Configures system-wide cron scheduling for weekday and weekend jobs.

### setup_aptconf.sh

Deploys system APT configuration for later site-specific editing.

### setup_apache2_ssl.sh

Configures Apache SSL support, certificates, and site settings. Requires review of host-specific configuration.

### setup_memcached_conf.sh

Deploys memcached configuration.

### setup_ntp_restart_policy.sh

Configures automatic restart-on-failure behavior for installed NTP services without immediately restarting them.

### setup_motd.sh

Configures the system message of the day.

### setup_rsyslog_cron.sh

Deploys cron logging configuration for rsyslog.

### setup_rsyslog_logrotate.sh

Deploys rsyslog log rotation configuration.

### setup_rsyslog_postfix.sh

Deploys Postfix logging configuration for rsyslog.

### setup_sysadmin_scripts.sh

Installs or removes system-administration scripts in system directories.

### setup_tune2fs.sh

Adjusts ext filesystem settings.

### purge_apt_cache.sh

Deletes residual APT configuration for removed packages. This is destructive.

### purge_kernels.sh

Removes obsolete Ubuntu kernel packages while preserving the running kernel.

### remove-tracker.sh

Disables and removes GNOME Tracker indexing. This changes desktop search functionality.

## 25. Google Chrome Installer

Browser installation through a package manager.

### install_google_chrome.sh

Installs Google Chrome Stable on Debian and configures its official APT source. Verifies the vendor signing key and leaves regular APT metadata refresh to normal package maintenance.

## 26. Automated Operations in cron/bin

Executables designed for scheduled execution. Installer scripts in §23 deploy many of these jobs.

### cron/bin/apache_log_analysis

Starts scheduled Apache log analysis.

### cron/bin/chkrootkit

Starts periodic rootkit scanning.

### cron/bin/clamscan

Starts ClamAV scanning.

### cron/bin/clamscan.sh

Performs the main ClamAV scan workflow.

### cron/bin/fix-permissions.sh

Repairs configured filesystem permissions on a schedule.

### cron/bin/get_resources

Generates scheduled system-resource reports.

### cron/bin/munin-symlink.sh

Monitors Munin symlink state.

### cron/bin/munin-sync.sh

Synchronizes Munin-related data on a schedule.

### cron/bin/rsync_backup

Starts scheduled removable-disk backups.

### cron/bin/rsync_backup.sh

Performs removable-disk backup and synchronization with device checks.

### cron/bin/run_tests

Runs the scheduled interpreter-matrix tests and repository checks.

## 27. Cron Configuration

`cron/etc/` supplies configuration, scheduling, and log rotation assets for periodic jobs. Its main files are indexed here to help identify the relevant configuration without documenting its internal settings.

| Asset type | Files |
| --- | --- |
| Job configuration | `clamscan.conf`, `fix-permissions.conf`, `munin-symlink.conf`, `munin-sync.conf`, `rsync_backup.conf`, `run_tests.conf` |
| Cron definitions | `cron.d/clamscan`, `cron.d/reboot`, `cron.d/shutdown` |
| Log rotation | `logrotate.d/apache2`, `logrotate.d/apache_summary`, `logrotate.d/chkrootkit`, `logrotate.d/clamscan`, `logrotate.d/fix-permissions`, `logrotate.d/resources`, `logrotate.d/rsync_backup`, `logrotate.d/run_tests` |

## 28. Support Configuration under etc/

`etc/` holds configuration and templates used by the repository's tools. The following index identifies major configuration groups and their representative files, without reproducing configuration contents.

| Purpose | Representative file or directory |
| --- | --- |
| Japanese input | `aa.txt` |
| Apache | `apache/mods-available/evasive.conf`, `apache/sites-available/hostname.sitename.conf`, `apache_ignore.list` |
| APT | `apt.conf`, `apt.conf.d/` |
| Dashcam | `dashcam_sync.conf` |
| fail2ban | `fail2ban/filter.d/apache-evasive.conf`, `fail2ban/jail.local` |
| Git archives | `git-archive-repo.conf` |
| GNOME | `gnome/gnome-shortcuts.conf`, `gnome/gnome-wm-keys.conf` |
| GPX | `gpx_sync.conf` |
| Instagram | `insta_sync.conf`, `insta_update.conf` |
| Firewall | `iptables/rules.v4` |
| Munin | `munin-apache.conf` |
| rsyslog | `rsyslog.d/10-cron.conf`, `rsyslog.d/30-postfix.conf` |
| SD-card extraction | `sd_extract.conf` |
| File transfer | `send_files.conf` |
| SSH | `sshd_config.d/000-sshdconfig.conf` |
| sudo | `sudoers`, `sudoers_macos` |
| Xfce | `xfce/terminalrc` |

## 29. dot_files/

`dot_files/` contains user configuration for shells, editors, source control, SSH, terminal utilities, Japanese input, and development tools. It also includes bundled third-party components, which are not automatically treated as distinct repository features.

## 30. Destructive or System-Wide Features

Some utilities delete data, rewrite repositories, modify authentication or network rules, or change persistent system settings. Check the script header and the selected target before use.

### File deletion

`remove-repo.sh`, `cltmp.sh`, `purge_apt_cache.sh`, and other cleanup tools can permanently remove data or configuration.

### Destructive Git operations

Some modes of `git-all-pull.sh`, `git-create-repo.sh`, and `remove-repo.sh` discard local work, delete branches, or remove repositories.

### Permission / ownership changes

`chmodtree.py`, `install_dotfiles.sh`, and setup scripts can change filesystem access and ownership.

### System configuration

Security and administration installers can alter authentication, firewall rules, services, and scheduled jobs.

### Package installation / removal

`installer/install_*.sh`, `installer/purge_*.sh`, and `installer/remove-tracker.sh` modify installed software and may affect services.

### TLS verification bypass

`install_vscode.sh`, `install_claude_desktop.sh`, and `install_claude_code.sh` offer an opt-in persistent APT TLS verification bypass. It weakens connection authentication and remains in effect until removed.

## 31. root and sudo

System-level installers generally require root or sudo privileges. Some development runtimes support a user-controlled installation prefix and `--no-sudo`, including `install_python.sh`, `install_ruby.sh`, `install_miniforge.sh`, `install_nodejs.sh`, `install_safe-chain.sh`, `install_zsh.sh`, and `install_ncurses.sh`. Consult each executable header for the exact requirements.

## 32. OS-Specific Behavior

The repository contains platform-specific and cross-platform tools. Individual executable headers define exact supported versions and architectures.

### Debian / Ubuntu based systems

APT maintenance, full-system setup, desktop provisioning, and security installers mainly target Debian or Ubuntu.

### macOS

Homebrew maintenance, Finder and user-environment setup, and macOS-specific administration tools target macOS.

### Linux

LUKS, systemd, firewall, and selected monitoring utilities target Linux systems.

### Multiple Unix-like platforms

Several source-build installers, file utilities, and Git tools work across Unix-like environments. Platform availability depends on each script.

## 33. Configuration File Handling

Features requiring site-specific settings use configuration under `etc/`, `cron/etc/`, or installed system paths. For example, `dashcam_sync.sh` uses `dashcam_sync.conf`, `git-archive-repo.sh` uses `git-archive-repo.conf`, `gpx_sync.sh` uses `gpx_sync.conf`, `sd_extract.sh` uses `sd_extract.conf`, and `send_files.sh` uses `send_files.conf`. Refer to the executable header and configuration file for exact settings.

## 34. Help and Header Documentation

Each executable header is the source for its exact usage, options, prerequisites, and exit statuses. Many scripts display this information through help or version options. Option support is script-specific and is not reproduced in full here.

## 35. Finding a Feature by Purpose

| Task | Relevant entry points |
| --- | --- |
| Format writing or HTML | `add_hr_h2.py`, `format_html.py`, `fix_anchor.py`, `aozora_prepare.rb` |
| Inspect files and directories | `els.py`, `dirsize.py`, `du.py`, `find_range.py` |
| Modify or clean file trees | `chmodtree.py`, `flatdirs.py`, `cleanup-junk-files.sh`, `swapext.py` |
| Manage Git repositories | `git-all-pull.sh`, `git-archive-repo.sh`, `git-create-repo.sh`, `remove-repo.sh` |
| Inspect disks and mountpoints | `get-device.sh`, `get-mountpoint.sh`, `get-serial.sh` |
| Access encrypted storage | `luksmount.py`, `tcmount.py` |
| Inspect system status | `get_resources.sh`, `check_reboot.sh`, `server_alive_check.sh` |
| Transfer or synchronize data | `send_files.sh`, `dashcam_sync.sh`, `gpx_sync.sh`, `sd_extract.sh` |
| Analyze web traffic | `apache_log_analysis.sh`, `apache_blog_analysis.py` |
| Validate and maintain code | `pyck.py`, `find_pycompat.py`, `check_header_doc.py`, `run_tests.sh` |
| Provision Debian or macOS | `installer/debian_init.sh`, `installer/macos_setup.sh` |
| Build language runtimes | `installer/install_python.sh`, `installer/install_ruby.sh`, `installer/install_zsh.sh`, `installer/install_ncurses.sh` |
| Install Conda and Node.js runtimes | `installer/install_miniforge.sh`, `installer/install_nodejs.sh` |
| Install development tools | `installer/install_npm.sh`, `installer/install_safe-chain.sh`, `installer/install_vscode.sh` |
| Install Claude clients | `installer/install_claude_desktop.sh`, `installer/install_claude_code.sh` |
| Deploy scheduled monitoring | `installer/install_clamscan.sh`, `installer/install_chkrootkit.sh`, `installer/install_munin.sh`, `installer/install_rsync_backup.sh` |
| Configure system security | `installer/setup_iptables.sh`, `installer/setup_pamd.sh`, `installer/setup_dos_guard.sh` |
| Manage package upgrades | `apt-upgrade.sh`, `brew-upgrade.sh`, `dpkg-hold.sh` |

## 36. Scope of This Document

This index identifies available tools, their primary purposes, their main supported contexts, and important operational risks. It does not replace the executable headers, source code, configuration documentation, or version history. A feature description belongs here when it helps a reader choose a tool or avoid a consequential misuse, not merely because a script has a particular internal implementation.
