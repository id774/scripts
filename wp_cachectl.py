#!/usr/bin/env python

########################################################################
# wp_cachectl.py: Safe WP-CLI Cache Control (Event-driven, Layered)
#
#  Description:
#  This script is a practical operations tool for WordPress cache handling.
#  It wraps WP-CLI to run cache operations in a predictable, event-driven way.
#
#  The key idea is simple:
#    - Cache clearing is not “periodic cleaning”.
#    - Cache clearing is an “event boundary operation”.
#      (e.g., after WordPress update, after theme change, troubleshooting)
#
#  In WordPress, caches exist in multiple layers. This script uses the following
#  layered model as a shared vocabulary, and applies operations from smaller
#  scope to larger scope.
#
#  Cache Layers (used by this tool):
#    - L1: Transients (WordPress built-in “temporary cached values” with TTL)
#    - L2: Object Cache (wp cache flush; can be broader impact depending on setup)
#    - L3: Page Cache Plugin (W3 Total Cache only, if available and active)
#    - L4: CDN/Browser cache (not automated; guidance only)
#
#  Safety model (intentionally minimal by request):
#    - Require an explicit target path (--path; default is /var/www/wordpress).
#    - Require explicit intent (--reason) for state-changing commands.
#    - Require explicit execution (--yes) unless --dry-run is used.
#    - Do not attempt to automate CDN/browser cache to avoid over-broad purges.
#
#  This tool does NOT try to detect or handle page cache plugins other than W3TC.
#  It does NOT implement allowlists, locks, or rate limits in this version.
#  (Those can be added later if operational needs grow.)
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python wp_cachectl.py [options] <command>
#
#  Commands:
#      status
#          Show detection results (WordPress reachable, home URL, W3TC active).
#
#      after-core-update
#          Post core-update steps:
#            - (optional) wp core update-db
#            - L1 -> L2 -> L3(if W3TC active)
#
#      after-plugin-update
#          Post plugin-update steps:
#            - default: L2 only
#            - optional: include L1 with --l1
#            - optional: include L3 with --l3-force
#
#      after-theme-update
#          Post theme/CSS update steps:
#            - L1 -> L2 -> L3(if W3TC active)
#
#      troubleshoot
#          Stepwise troubleshooting. Requires --level 1..4.
#            1: L1 only
#            2: L1 -> L2
#            3: L1 -> L2 -> L3(if W3TC active)
#            4: Print L4 guidance only
#
#  Options:
#      -p, --path:       WordPress install path (default: /var/www/wordpress).
#      -r, --reason:     Reason for execution (required except for status).
#      -n, --dry-run:    Print planned actions only (no changes).
#      -y, --yes:        Execute changes (required unless --dry-run).
#      --update-db:      Run 'wp core update-db' (after-core-update only).
#      --l1:             Include L1 transients deletion (after-plugin-update only).
#      --l3-force:       Include L3 W3TC flush (after-plugin-update only).
#      --level:          Troubleshoot level (1..4).
#      -v, --version:    Show version and exit (extracted from header).
#
#  Requirements:
#  - Python Version: 3.1 or later
#  - WP-CLI available as 'wp' in PATH (or set WP_BIN environment variable)
#
#  Exit Status:
#  - 0: Success
#  - 1: Execution failed (WP-CLI command failed)
#  - 2: Usage error (missing args, invalid command/option)
#  - 4: Not a WordPress install (or cannot access)
#  - 5: WP-CLI not found
#  - 9: Unsupported Python version
#
#  Version History:
#  v1.0 2026-01-29
#       Initial release. Event-driven cache operations with W3TC support.
#
########################################################################

import os
import subprocess
import sys
from optparse import OptionParser

try:
    # Python 3.1+ has urllib.parse
    from urllib.parse import urlparse
except Exception:
    urlparse = None

__version__ = "unknown"  # will be overwritten in main()


EX_OK = 0
EX_RUN = 1
EX_USAGE = 2
EX_NOTWP = 4
EX_NOWPCLI = 5
EX_PYVER = 9


DEFAULT_WP_PATH = "/var/www/wordpress"


def usage():
    """Display the script header as usage information and exit."""
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, "r", encoding="utf-8") as f:
            for line in f:
                if line.strip().startswith("#" * 10):
                    if not in_header:
                        in_header = True
                        continue
                    break
                if in_header and line.startswith("#"):
                    if line.startswith("# "):
                        print(line[2:], end="")
                    else:
                        print(line[1:], end="")
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(EX_USAGE)
    sys.exit(EX_OK)


def get_script_version():
    """Extract the script version from the header comment block."""
    script_path = os.path.abspath(__file__)
    found_history = False
    try:
        with open(script_path, "r", encoding="utf-8") as f:
            for line in f:
                if "Version History" in line:
                    found_history = True
                    continue
                if found_history and line.strip().startswith("#  v"):
                    # Example: "#  v1.0 2026-01-29"
                    parts = line.strip().split()
                    if len(parts) >= 2:
                        return parts[1]
                    return "unknown"
    except Exception:
        return "unknown"
    return "unknown"


def command_exists(command):
    """Check whether a command exists in PATH using 'command -v'."""
    with open(os.devnull, "w") as devnull:
        return subprocess.call(
            "command -v %s" % command,
            shell=True,
            stdout=devnull,
            stderr=devnull,
        ) == 0


def log_info(msg):
    """Print an informational message."""
    print("[INFO] %s" % msg)


def log_warn(msg):
    """Print a warning message."""
    print("[WARN] %s" % msg)


def log_error(msg):
    """Print an error message to stderr."""
    print("[EROR] %s" % msg, file=sys.stderr)


def die(code, msg):
    """Terminate execution with an error."""
    log_error(msg)
    sys.exit(code)


def must_yes_or_dryrun(yes, dry_run):
    """Refuse state-changing operations unless --yes or --dry-run is provided."""
    if dry_run:
        return
    if not yes:
        die(EX_USAGE, "Refusing to run without --yes (or use --dry-run).")


def wp_argv(wp_bin, wp_path, args):
    """Build WP-CLI argv with --path."""
    return [wp_bin, "--path=%s" % wp_path] + list(args)


def run_wp(argv, dry_run):
    """Run WP-CLI command and return exit code."""
    if dry_run:
        log_info("DRY-RUN: %s" % " ".join(argv))
        return 0

    log_info("RUN: %s" % " ".join(argv))
    return subprocess.call(argv)


def wp_try(wp_bin, wp_path, args):
    """Run WP-CLI quietly and return True if success."""
    with open(os.devnull, "w") as devnull:
        return subprocess.call(
            wp_argv(wp_bin, wp_path, args),
            stdout=devnull,
            stderr=devnull,
        ) == 0


def ensure_wpcli(wp_bin):
    """Ensure WP-CLI is available."""
    if not command_exists(wp_bin):
        die(EX_NOWPCLI, "WP-CLI not found: %s" % wp_bin)


def ensure_wordpress(wp_bin, wp_path):
    """Ensure the target path is a WordPress install."""
    if not wp_try(wp_bin, wp_path, ["core", "is-installed"]):
        die(EX_NOTWP, "Not a WordPress install (or cannot access): %s" % wp_path)


def get_home_url(wp_bin, wp_path):
    """Get WordPress home URL (best-effort)."""
    try:
        out = subprocess.check_output(
            wp_argv(wp_bin, wp_path, ["option", "get", "home"]),
            stderr=subprocess.STDOUT,
        )
        return out.decode("utf-8", errors="replace").strip()
    except Exception:
        return ""


def parse_host(url):
    """Extract host from URL safely (best-effort)."""
    if not url:
        return ""
    if urlparse is None:
        return ""
    try:
        p = urlparse(url)
        return p.hostname or ""
    except Exception:
        return ""


def detect_w3tc(wp_bin, wp_path):
    """Detect W3 Total Cache via two-step check (command exists AND plugin active)."""
    cmd_exists = wp_try(wp_bin, wp_path, ["help", "w3-total-cache"])
    active = wp_try(wp_bin, wp_path, ["plugin", "is-active", "w3-total-cache"])
    return cmd_exists and active


def do_l1_transients(wp_bin, wp_path, dry_run):
    """Delete transients (L1)."""
    log_info("L1: Delete transients (wp transient delete --all)")
    rc = run_wp(wp_argv(wp_bin, wp_path, ["transient", "delete", "--all"]), dry_run)
    if rc != 0:
        die(EX_RUN, "Failed L1: transients deletion (rc=%d)" % rc)


def do_l2_object_cache(wp_bin, wp_path, dry_run):
    """Flush object cache (L2)."""
    log_info("L2: Flush object cache (wp cache flush)")
    rc = run_wp(wp_argv(wp_bin, wp_path, ["cache", "flush"]), dry_run)
    if rc != 0:
        die(EX_RUN, "Failed L2: object cache flush (rc=%d)" % rc)


def do_l3_w3tc(wp_bin, wp_path, dry_run):
    """Flush W3 Total Cache (L3) if active; otherwise skip."""
    if detect_w3tc(wp_bin, wp_path):
        log_info("L3: Flush W3 Total Cache (wp w3-total-cache flush all)")
        rc = run_wp(wp_argv(wp_bin, wp_path, ["w3-total-cache", "flush", "all"]), dry_run)
        if rc != 0:
            die(EX_RUN, "Failed L3: W3TC flush (rc=%d)" % rc)
        return

    log_warn("L3: W3 Total Cache not active/available; skip page cache flush")


def print_l4_guidance():
    """Print guidance for CDN/browser cache (L4)."""
    log_info("L4: CDN/Browser cache is not automated by this tool.")
    log_info("If a CDN is used, prefer purging by URL (narrow purge) rather than full purge.")
    log_info("If only one client sees stale content, test in private window / another device/network.")


def cmd_status(wp_bin, wp_path):
    """Print status information for the target WordPress."""
    ensure_wordpress(wp_bin, wp_path)

    home_url = get_home_url(wp_bin, wp_path)
    host = parse_host(home_url)

    log_info("WP_BIN: %s" % wp_bin)
    log_info("WP_PATH: %s" % wp_path)
    log_info("HOME_URL: %s" % (home_url if home_url else "(unknown)"))
    log_info("HOST: %s" % (host if host else "(unparsed)"))

    if detect_w3tc(wp_bin, wp_path):
        log_info("W3TC: active + command available")
    else:
        log_warn("W3TC: not active or command missing")

    log_info("Event behavior summary:")
    log_info("  after-core-update:   (optional update-db) L1 -> L2 -> L3(if W3TC) + L4 guidance")
    log_info("  after-plugin-update: default L2; optional L1; optional L3 with --l3-force")
    log_info("  after-theme-update:  L1 -> L2 -> L3(if W3TC) + L4 guidance")
    log_info("  troubleshoot:        stepwise with --level 1..4")


def cmd_after_core_update(wp_bin, wp_path, dry_run, update_db):
    """Run post core-update steps."""
    if update_db:
        log_info("Running DB update (wp core update-db)")
        rc = run_wp(wp_argv(wp_bin, wp_path, ["core", "update-db"]), dry_run)
        if rc != 0:
            die(EX_RUN, "Failed core update-db (rc=%d)" % rc)

    do_l1_transients(wp_bin, wp_path, dry_run)
    do_l2_object_cache(wp_bin, wp_path, dry_run)
    do_l3_w3tc(wp_bin, wp_path, dry_run)
    print_l4_guidance()


def cmd_after_plugin_update(wp_bin, wp_path, dry_run, include_l1, l3_force):
    """Run post plugin-update steps."""
    if include_l1:
        do_l1_transients(wp_bin, wp_path, dry_run)
    else:
        log_info("L1: skipped (default). Use --l1 to include if needed.")

    do_l2_object_cache(wp_bin, wp_path, dry_run)

    if l3_force:
        do_l3_w3tc(wp_bin, wp_path, dry_run)
    else:
        log_info("L3: skipped (default). Use --l3-force to include if needed.")

    print_l4_guidance()


def cmd_after_theme_update(wp_bin, wp_path, dry_run):
    """Run post theme/CSS update steps."""
    do_l1_transients(wp_bin, wp_path, dry_run)
    do_l2_object_cache(wp_bin, wp_path, dry_run)
    do_l3_w3tc(wp_bin, wp_path, dry_run)
    print_l4_guidance()


def cmd_troubleshoot(wp_bin, wp_path, dry_run, level):
    """Run stepwise troubleshooting by level."""
    if level not in (1, 2, 3, 4):
        die(EX_USAGE, "Invalid --level: %s (expected 1..4)" % str(level))

    log_info("Troubleshoot level: %d" % level)

    if level == 4:
        print_l4_guidance()
        return

    do_l1_transients(wp_bin, wp_path, dry_run)

    if level >= 2:
        do_l2_object_cache(wp_bin, wp_path, dry_run)

    if level >= 3:
        do_l3_w3tc(wp_bin, wp_path, dry_run)

    print_l4_guidance()


def main():
    """Parse arguments and execute commands."""
    global __version__
    __version__ = get_script_version()

    wp_bin = os.environ.get("WP_BIN", "wp")

    parser = OptionParser("usage: %prog [options] <command>", version="wp_cachectl.py %s" % __version__)
    parser.add_option("-p", "--path",
                      dest="wp_path",
                      help="WordPress install path (default: %s)" % DEFAULT_WP_PATH,
                      action="store",
                      type="string",
                      default=DEFAULT_WP_PATH)
    parser.add_option("-r", "--reason",
                      dest="reason",
                      help="reason for execution (required except status)",
                      action="store",
                      type="string",
                      default="")
    parser.add_option("-n", "--dry-run",
                      dest="dry_run",
                      help="dry run (no changes)",
                      action="store_true",
                      default=False)
    parser.add_option("-y", "--yes",
                      dest="yes",
                      help="execute changes",
                      action="store_true",
                      default=False)
    parser.add_option("--update-db",
                      dest="update_db",
                      help="run wp core update-db (after-core-update)",
                      action="store_true",
                      default=False)
    parser.add_option("--l1",
                      dest="include_l1",
                      help="include L1 transient deletion (after-plugin-update)",
                      action="store_true",
                      default=False)
    parser.add_option("--l3-force",
                      dest="l3_force",
                      help="include L3 W3TC flush (after-plugin-update)",
                      action="store_true",
                      default=False)
    parser.add_option("--level",
                      dest="level",
                      help="troubleshoot level (1..4)",
                      action="store",
                      type="int",
                      default=None)

    (options, args) = parser.parse_args()

    if not args:
        return EX_USAGE

    cmd = args[0]
    ensure_wpcli(wp_bin)
    ensure_wordpress(wp_bin, options.wp_path)

    if cmd == "status":
        cmd_status(wp_bin, options.wp_path)
        return EX_OK

    # State-changing commands require --reason and --yes (unless --dry-run).
    if not options.reason:
        die(EX_USAGE, "--reason is required for this command")
    must_yes_or_dryrun(options.yes, options.dry_run)

    log_info("Command: %s" % cmd)
    log_info("Reason: %s" % options.reason)
    log_info("Target path: %s" % options.wp_path)

    if cmd == "after-core-update":
        cmd_after_core_update(wp_bin, options.wp_path, options.dry_run, options.update_db)
        return EX_OK

    if cmd == "after-plugin-update":
        cmd_after_plugin_update(wp_bin, options.wp_path, options.dry_run, options.include_l1, options.l3_force)
        return EX_OK

    if cmd == "after-theme-update":
        cmd_after_theme_update(wp_bin, options.wp_path, options.dry_run)
        return EX_OK

    if cmd == "troubleshoot":
        if options.level is None:
            die(EX_USAGE, "--level is required for troubleshoot")
        cmd_troubleshoot(wp_bin, options.wp_path, options.dry_run, options.level)
        return EX_OK

    die(EX_USAGE, "Unknown command: %s" % cmd)
    return EX_USAGE


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ("-h", "--help"):
        usage()

    if sys.version_info < (3, 1):
        log_error("This script requires Python 3.1 or later.")
        sys.exit(EX_PYVER)

    sys.exit(main())
