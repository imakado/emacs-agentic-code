# emacs-agentic-code

<p align="center">
  <a href="https://github.com/imakado/emacs-agentic-code/releases">
    <img src="https://img.shields.io/badge/version-0.1.0-blue.svg" alt="Version">
  </a>
  <a href="https://www.gnu.org/licenses/gpl-3.0">
    <img src="https://img.shields.io/badge/License-GPL%20v3-blue.svg" alt="License: GPL v3">
  </a>
  <a href="https://www.gnu.org/software/emacs/">
    <img src="https://img.shields.io/badge/Emacs-27.1%2B-purple.svg" alt="Emacs 27.1+">
  </a>
</p>

<p align="center">
  <b>Vibe Coding with Claude Code in Emacs</b>
</p>

## 🎬 demo

<p align="center">
  <i>Demo video will be uploaded within a few days.</i>
</p>

## 📚 Table of Contents

- [Why Agentic Code](#why-agentic-code)
- [Quick Start](#quick-start)
- [Key Features](#key-features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Key Bindings](#key-bindings)
- [Customization](#customization)
- [Contributing](#contributing)
- [License](#license)
- [Author](#author)
- [Acknowledgments](#acknowledgments)

## Why I Created Agentic Code

### 🚧 Challenge: Complex Input in Terminal

[Claude Code](https://claude.ai/code) is an innovative AI coding assistant, but its terminal-based interface has significant limitations:

- **Difficulty with languages requiring Input Method Editors (IME)** - Long text input in languages that require character conversion (Japanese, Chinese, Korean, etc.) is extremely stressful
- **Cumbersome multi-line editing** - Multi-line prompt editing in terminal is not efficient
- **Tedious switching between multiple projects**

### ✨ Solution: Native Emacs Integration

Agentic Code solves these challenges with Emacs's powerful editing capabilities:

- **Native input experience** - Natural text input in Emacs buffers, with full IME support
- **Dedicated editing buffer** - Comfortable editing environment with Markdown highlighting
- **Smart file references** - Easy file selection and submission with Helm integration
- **Efficient switching between multiple projects**
- **Stable window layout**

## Quick Start

### My Configuration

```elisp
(require 'agentic-code)

;;; Customization
;; Display the project route in the terminal buffer header.
(setq agentic-code-show-project-root-in-header t)

;;; Key Binding
;; Assign the menu used with transient.el to a key binding.
;; I use "bind-key*" to ensure that this key binding takes precedence in any location.
;; Be careful, because it may overwrite key bindings in other modes.
(require 'bind-key)
(bind-key* "M-SPC" 'agentic-code-transient-menu)

;; Recommended: Set global key bindings
(global-set-key (kbd "M-t") agentic-code-command-map)

;; My recommended key bindings for working with Claude Code in vterm.el
(keymap-set agentic-code-vterm-minor-mode-map
            "C-n"
            "<down>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-p"
            "<up>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-y"
            'vterm-yank)

;;; Window Configuration
(require 'agentic-code-window-layout-config)
(agentic-code-layout-setup-3-columns)
;; OR If you use a laptop
;; (agentic-code-layout-setup-2-columns)
```

### Basic Usage

**🚀 Just remember this to get started!**

```
M-SPC  ; Open Transient menu (access to all features)
```

First-time users should start by pressing `M-SPC` (Meta + Space) to open the menu.
You can utilize the AI assistant by simply selecting the necessary features from the visual menu.

#### Common Operation Examples:
1. `M-SPC` → `o` : Start AI assistant in project
2. `M-SPC` → `f` : Send current file to AI
3. `M-SPC` → `h` : Switch between multiple sessions
4. `M-SPC` → `l` → `3` : Change to 3-column layout


## Key Features

### 🌟 Native Emacs Experience

#### Comfortable Input Environment
- **Natural editing in Emacs buffers** - Familiar key bindings and IME support
- **Markdown syntax highlighting** - Improved visibility when creating prompts
- **Freedom of multi-line editing** - Liberation from terminal constraints

#### Seamless Integration
- **Full integration via vterm.el** - Run Claude Code CLI directly within Emacs buffers
- **Project awareness** - Automatically detects project root with Projectile integration
- **Helm integration** - Batch selection of multiple files → send file references, select multiple sessions → display, and other feature implementations

### 📁 Intelligent Session Management

- **Independent sessions per project** - Dedicated AI assistant session for each project
- **Automatic restoration** - Remember previous work state and resume immediately
- **Multi-project support** - Handle multiple projects simultaneously, switch instantly with `C-c a h`
- **Window configuration saving** - Automatically remember layout for each project

### 📝 Smart File Reference System

- **DWIM (Do What I Mean) behavior** - Optimal behavior according to context
- **File selection with Helm** - Quickly select files within the project
- **Batch submission of multiple files** 

### 🪟 Optimized Window Layouts

Two layouts to choose from based on your work style:

```
2-column layout (for simple work):
┌─────────────┬─────────────┐
│   Context   │   Claude    │
│   (50%)     │   (50%)     │
└─────────────┴─────────────┘

3-column layout (for complex work):
┌───────┬───────┬───────────┐
│History│Context│  Claude   │
│ (33%) │ (33%) │   (34%)   │
└───────┴───────┴───────────┘
```

- **Dynamic layout switching** - Instantly change layout according to work content
- **Automatic placement of editing buffer** - Automatically displayed at the bottom during prompt editing
- **Window configuration cycling** - Cycle through layouts between sessions with `C-c a w`

### 🎯 Intuitive Transient Menu

Magit-style menu system for quick access to all features.

#### 📖 How to Read the Menu

When you press `M-SPC`, a menu with the following structure is displayed:

```
Claude Code Sessions | Project name (+ 2 more)  ← Current session status

Session Management        File & Editing           Layout & UI
o Start in project root   f Send file reference    l Layout management →
d Start in directory...   p Pick project files     m Open this menu
h Select session [3]      c Copy file reference    
w Cycle window configs    e Edit in buffer         q Quit
```

#### 🔤 Key Mnemonics

Menu keys are designed intuitively:
- **o** : Open (open in project)
- **f** : File (send file)
- **h** : Helm (session selection)
- **p** : Pick (select and send files)
- **e** : Edit (editing buffer)
- **l** : Layout (layout management)

#### ✨ Convenient Features

- **Visual status display** - Check active session count at a glance
- **Context-dependent menu** - Display appropriate options according to current state
- **Filename display** - Operation target files are displayed in `[filename]` format
- **Submenus** - Press `l` to go to layout management submenu


## Prerequisites

### Development Environment

- **Emacs**: 27.1 or later (Emacs 30 recommended)
- **Claude Code**: Install from [official website](https://docs.anthropic.com/en/docs/claude-code/overview)

## Installation
### use-package
```elisp
;; ---------- Emacs 30+  --------------
(use-package agentic-code
  :vc (:url "https://github.com/imakado/emacs-agentic-code.git"
       :rev :newest
       :main-file "agentic-code.el")
  :after (projectile magit)
  :commands (agentic-code:project-root))
```

### straight.el

```elisp
(straight-use-package
 '(agentic-code :type git
                :host github
                :repo "imakado/emacs-agentic-code"))
```


## Usage

### 🎨 Getting Started with Transient Menu (Recommended)

For first-time users, we recommend using the Transient menu with `M-SPC`.
Since all features are visually displayed, there's no need to memorize commands.

For configuration, see "My Configuration" in the [Quick Start](#quick-start) section.

### 📋 Main Operation Guide

```elisp
(require 'agentic-code)

;;; Customization
;; Display the project route in the terminal buffer header.
(setq agentic-code-show-project-root-in-header t)

;;; Key Binding
;; Assign the menu used with transient.el to a key binding.
;; I use "bind-key*" to ensure that this key binding takes precedence in any location.
;; Be careful, because it may overwrite key bindings in other modes.
(require 'bind-key)
(bind-key* "M-SPC" 'agentic-code-transient-menu)

;; Recommended: Set global key bindings
(global-set-key (kbd "M-t") agentic-code-command-map)

;; My recommended key bindings for working with Claude Code in vterm.el
(keymap-set agentic-code-vterm-minor-mode-map
            "C-n"
            "<down>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-p"
            "<up>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-y"
            'vterm-yank)

;;; Window Configuration
(require 'agentic-code-window-layout-config)
(agentic-code-layout-setup-3-columns)
;; OR If you use a laptop
;; (agentic-code-layout-setup-2-columns)
```


#### 1. Starting the AI Assistant

**🌟 From Transient Menu (Recommended):**
```
M-SPC → o  ; Start in project root
M-SPC → d  ; Start in any directory
```

On first launch, a vterm buffer is automatically created and Claude Code starts.
When executed again in the same project, it switches to the existing session.

#### 2. Sending File References

**🌟 From Transient Menu:**
```
M-SPC → f  ; Send current file or selected region
M-SPC → p  ; Select and send project files with Helm (multiple selection available)
M-SPC → c  ; Copy file reference to clipboard
```

💡 **Tip**: Using `p`, you can select multiple files in the Helm interface.
Mark with `C-SPC`, select all with `M-a`.

#### 3. Using the Edit Buffer

Within vterm buffer:
```
C-c C-e  ; Open edit buffer
```

Within edit buffer:
```
C-c C-c  ; Send content and close
C-c C-k  ; Cancel and close
```

#### 4. Session and Layout Management

**🌟 From Transient Menu:**
```
M-SPC → h  ; Select active session (using Helm)
M-SPC → w  ; Cycle window configurations
M-SPC → l  ; Go to layout management submenu
  → 2      ; 2-column layout
  → 3      ; 3-column layout  
  → r      ; Reset layout
```

💡 **Tip**: Press `w` continuously to cycle through window configurations of each session.

### Practical Workflow Examples

#### Working with Multiple Projects

1. Start AI assistant in project A: `M-SPC → o`
2. Move to another project B and start: `M-SPC → o`
3. Switch between sessions: `M-SPC → h`
4. Cycle window configurations: `M-SPC → w` (press `w` continuously to switch)

#### Creating Complex Prompts

1. Start Claude Code in vterm
2. Open edit buffer with `C-c C-e`
3. Add multiple file references:
   ```
   Please implement a new feature based on the following files:
   
   @src/main.el
   @src/utils.el#L20-50
   @tests/test-main.el
   
   Requirements:
   - Add error handling
   - Create unit tests
   - Update documentation
   ```
4. Send with `C-c C-c`

## Key Bindings

### 🌟 Recommended: Transient Menu (For Beginners)

**Just remember this to get started!**

```elisp
;; Open menu with Meta + Space
(bind-key* "M-SPC" 'agentic-code-transient-menu)
```

Press `M-SPC` to display a visual menu with access to all features.
No need to memorize commands, you can operate intuitively.

#### Main Transient Menu Keys

| Key | Function | Description |
|-----|----------|-------------|
| `o` | Start in project | Start Claude Code in current project root |
| `d` | Start in directory | Choose any directory and start |
| `f` | Send file | Send current file/selected range |
| `p` | Pick and send files | Select multiple files with Helm and send |
| `h` | Select session | Select from active session list |
| `w` | Cycle windows | Cycle through session window configurations |
| `l` | Layout management | Window layout submenu |
| `e` | Edit buffer | Open buffer for long text input in vterm |
| `?` | Help | Display key binding list |

### 📎 Optional: Command Map (For Advanced Users)

If you prefer traditional Emacs-style key bindings:

```elisp
;; Use M-t as prefix key
(global-set-key (kbd "M-t") agentic-code-command-map)
```

#### Command Map Key Binding List

| Key | Command | Description |
|-----|---------|-------------|
| `M-t o` | `agentic-code-start-claude-code-project-root` | Start in project root |
| `M-t d` | `agentic-code-start-claude-code` | Start in any directory |
| `M-t h` | `agentic-code-helm-select-session` | Select session |
| `M-t w` | `agentic-code-cycle-window-configurations` | Cycle window configurations |
| `M-t f` | `agentic-code-send-claude-file-reference-dwim` | Send file reference |
| `M-t p` | `agentic-code-helm-projectile-send-files` | Select and send project files |
| `M-t c` | `agentic-code-claude-file-reference-string-to-clipboard` | Copy reference |
| `M-t e` | `agentic-code-edit-in-buffer` | Open edit buffer |
| `M-t m` | `agentic-code-transient-menu` | Open Transient menu |
| `M-t l 2` | `agentic-code-layout-setup-2-columns` | 2-column layout |
| `M-t l 3` | `agentic-code-layout-setup-3-columns` | 3-column layout |
| `M-t l r` | `agentic-code-layout-reset` | Reset layout |

## Customization

### Main Customization Variables

```elisp
;; Claude CLI command path
(setq agentic-code-cli-command "claude")

;; Display project root in header
(setq agentic-code-show-project-root-in-header t)

;; Auto-delete Claude Code initial suggestions
(setq agentic-code-remove-claude-code-suggestion t)

;; Window layout settings
(setq agentic-code-layout-window-height-ratio 0.3)
(setq agentic-code-layout-2col-left-size-ratio 0.5)
(setq agentic-code-layout-3col-left-size-ratio 0.33)

;; Log level settings (for debugging)
(setq agentic-code-log-level 'info) ; 'debug, 'info, 'warn, 'error, 'fatal, nil
```

### Custom Faces

```elisp
;; Header project label
(set-face-attribute 'agentic-code-header-project-label-face nil
                    :weight 'bold
                    :height 1.2)

;; Header project path
(set-face-attribute 'agentic-code-header-project-path-face nil
                    :foreground "cyan"
                    :height 1.1)
```

## Contributing

### Bug Reports and Feature Requests

[GitHub Issues](https://github.com/imakado/emacs-agentic-code/issues)

### Coding Standards

Please follow the Emacs Lisp coding standards described in [CLAUDE.md](./CLAUDE.md).

## License

This project is licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0).

## Author

**imakado**
- GitHub: [@imakado](https://github.com/imakado)
- X: [@imakado](https://x.com/imakado)

## Acknowledgments

This project is inspired by the following projects:

- [vterm.el](https://github.com/akermu/emacs-libvterm) - High-performance terminal emulator
- [Claude Code](https://www.anthropic.com/claude-code) - Anthropic's innovative AI coding assistant

We thank the Emacs community for their continued support and contributions.