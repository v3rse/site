# Personal Site

My personal site built with [org-personal-site](https://github.com/v3rse/org-personal-site), a static site generator for Emacs org-mode.

## Publishing

This site is built from org-mode files and published to GitHub Pages. The build process is managed entirely from Emacs using the org-personal-site package.

### Workflow

```elisp
;; Create new post
M-x org-personal-site-new-post

;; Preview with live reload
M-x org-personal-site-preview

;; Deploy to GitHub Pages
M-x org-personal-site-deploy
```

### Build System

The site uses a custom Emacs package that:
- Converts org-mode files to HTML using `ox-publish`
- Generates CSS from theme configuration
- Creates RSS feed automatically
- Injects recent posts into homepage
- Provides live preview with file watching
- Deploys via git to GitHub Pages

All without leaving Emacs!

## Design Philosophy

### Inspirations

This site draws inspiration from several personal web projects:

- **[XXIIVV](https://wiki.xxiivv.com/)** by Devine Lu Linvega - Terminal aesthetics, knowledge organization, and the idea of a personal wiki as a living document
- **[100 Rabbits](https://100r.co/)** - Minimalism, self-hosted tools, and documentation-driven design
- **[Gwern.net](https://gwern.net/)** - Long-form writing, thoughtful organization, and treating a personal site as an evolving knowledge base
- **[Marginalia](https://www.marginalia.nu/)** - Simple, text-focused design and the web as a place for ideas
- **[Sourcehut](https://sourcehut.org/)** - Terminal-inspired aesthetics and brutalist simplicity

### Theme: Opencode TUI

The site uses a dark, warm, terminal-inspired theme:
- **Background**: Warm black (#131010)
- **Accent**: Warm orange/brown (#d4a574)
- **Fonts**: Pixelify Sans (logo), JetBrains Mono (headings), Iosevka Etoile (body)
- **Philosophy**: Clean, readable, monospace-forward design that feels like a terminal but remains accessible

The org-personal-site package includes three built-in themes:

1. **Opencode TUI** (default) - Dark, warm, terminal-inspired
   - Colors: #131010 (bg), #d4a574 (accent)
   - Inspired by terminal UIs and code editors
   - Warm tones reduce eye strain

2. **Light** - Clean light theme
   - Colors: #ffffff (bg), #0066cc (accent)
   - For those who prefer light backgrounds
   - High contrast, accessible

3. **Gruvbox** - Popular retro groove theme
   - Colors: #282828 (bg), #fe8019 (accent)
   - Warm, earthy tones
   - Based on the [Gruvbox color scheme](https://github.com/morhetz/gruvbox)

You can also create custom themes by specifying your own colors:

```elisp
(setq org-personal-site-theme
      '(:background "#1e1e1e"
        :foreground "#d4d4d4"
        :accent "#569cd6"))
```

The theme system is intentionally minimal - just background, foreground, and accent colors. This constraint encourages thoughtful color choices and keeps the design cohesive.

### Why This Aesthetic?

The terminal-inspired design isn't just nostalgia - it's intentional:

- **Monospace fonts** aid reading technical content and code
- **Dark backgrounds** reduce eye strain for long reading sessions
- **Warm tones** (#d4a574) feel more human than cold blues
- **High contrast** ensures accessibility
- **No distractions** - pure content, no sidebars or widgets
- **Familiar to coders** - if you use a terminal, this feels like home

The design says: "This is a place for thinking and building, not consuming."

### Principles

1. **Frictionless writing** - The barrier between thought and published content should be minimal
2. **Org-mode native** - Use Emacs and org-mode as the primary authoring environment
3. **No JavaScript** - Fast, accessible, works everywhere
4. **Minimal dependencies** - Self-contained build system, few external dependencies
5. **Version controlled** - Everything in git, including the build tools
6. **Local-first** - Build and preview locally, deploy when ready

## Structure

```
content/
├── index.org          # Homepage (bio + recent posts)
├── projects.org       # Projects showcase
├── blog/              # Blog posts
│   ├── 2026-02-01-opencode.org
│   ├── 2025-10-05-first-post.org
│   └── ...
└── media/             # Images and assets
```

## Technology

- **Generator**: [org-personal-site](https://github.com/v3rse/org-personal-site) (custom Emacs package)
- **Format**: Org-mode
- **Hosting**: GitHub Pages
- **Domain**: [Custom domain via CNAME]
- **Editor**: Emacs
- **Version Control**: Git

## Building from Source

If you want to build a similar site:

1. Install the [org-personal-site](https://github.com/v3rse/org-personal-site) package
2. Create a `content/` directory with your org files
3. Configure the package in your Emacs config
4. Build with `M-x org-personal-site-build`

See the package documentation for details.

## Philosophy on Personal Websites

I believe personal websites should be:

- **Yours** - Full control over design, content, and publishing
- **Simple** - Fast to load, easy to maintain, accessible to all
- **Timeless** - Plain text and HTML that will work for decades
- **Living** - A garden to tend, not a monument to build once
- **Hackable** - Tools you can modify and understand

This site is an experiment in that direction - a place to think, write, and share without platform constraints or algorithms deciding what gets seen.

## License

Content is my own. Site generator is open source.

---

*Last updated: 2026-02-01*
