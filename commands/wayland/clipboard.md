---
allowed-tools: Bash
description: Copy or paste using Wayland clipboard
argument-hint: <copy|paste> [text]
---

# Wayland Clipboard

Perform clipboard operation:

$ARGUMENTS

Commands:
- Copy: `echo "text" | wl-copy`
- Paste: `wl-paste`
- Copy file: `wl-copy < file.txt`
