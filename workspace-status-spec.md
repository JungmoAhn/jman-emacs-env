# Workspace Status Mode (Magit-style) — Spec

## Purpose
Magit status처럼 **전용 status 버퍼**에서 Org 업무를 정리한다.

- Storage: Org files 그대로
- View/Actions: `magit-section` 기반
- `TAB`=파일, `RET`=엔트리 위치

---

## Sections
Order:
1. Inbox (#N)
2. Tasks (#N)
3. Projects Active (#N)
4. Projects Maint (#N)
5. Projects Archived (#N)
6. Projects Blocked (#N) — Hybrid
   - Missing project homes (#N)
7. Topics (#N)

---

## Keybindings
Global:
- `g` refresh
- `?` dispatch/help (transient if available)
- `TAB` open file (file-level)
- `RET` visit entry (entry-level)
- `z`/`Z` toggle section / all sections folding

Item actions:
- `s` stage (Inbox/Tasks → Project)
- `S` stage all (current section)
- `u` fallback chain (Project task → Tasks → Inbox)
- `U` force to Inbox
- `b` toggle BLOCK (TODO ↔ BLOCK)
- `k` archive/delete subtree

Project actions:
- `a` active, `m` maint, `A` archived
- `B` toggle manual blocked (`proj_blocked`)
- `x` show blockers (project file)
- `v` tags view for `proj_tag`
- `C` toggle auto-block scope (`project-file` ↔ `agenda-tag`)

Missing-project actions:
- `p` create project home for `proj_tag`

---

## Definitions

### Inbox
Source: `workspace-status-inbox-file` (default `~/org/inbox.org`) — all TODO entries.

### Tasks
Source: `workspace-status-tasks-file` (default `~/org/tasks.org`) — all TODO entries.

### Projects (home files)
Source: `workspace-status-projects-dir` (default `~/org/roam/projects`).
A file is a project home if its `#+filetags:` contains `:project:`.
Status tags:
- `proj_active`
- `proj_maint`
- `proj_archived`
- `proj_blocked` (manual blocked)

Each project line may show health: `NEXT/TODO/BLOCK` counts; warning if `NEXT=0` while `TODO>0`.

### Projects Blocked (Hybrid)
A project is shown in Blocked if:
- **Manual**: filetags include `proj_blocked` ⇒ label `[M]`
- **Auto**: has BLOCK tasks ⇒ label `[A]` (and `(N blockers)`)
- both ⇒ `[M+A]`

Priority: Blocked overrides Active/Maint/Archived (no duplication).

Auto-block scope:
- `project-file` (default): only scan project home file
- `agenda-tag`: scan `org-agenda-files` for BLOCK entries that also have the project’s `proj_...` tag

### Missing project homes
If Inbox/Tasks contain a `proj_...` tag that does **not** exist among project homes, it appears here.

### Topics
Files in `workspace-status-topic-search-dirs` are scanned;
- Always include files under `workspace-status-topics-dir`
- Additionally include any file whose `#+filetags:` contains `:topic:`.

---

## Stage / Fallback semantics
- **Stage (`s`)**: cut subtree from Inbox/Tasks and paste under Project’s destination heading (default `Tasks`).
  - If item has `proj_...` tag, prefer that project; otherwise prompt.
- **Fallback (`u`)**:
  - project-task → Tasks file
  - task → Inbox file

---

## Acceptance tests
1. Inbox/Tasks show correct counts and open with TAB/RET.
2. Stage (`s`) moves entry to selected project file under `Tasks`.
3. Project’s Next/Blockers sublists are navigable, and `u` moves them back to Tasks.
4. Hybrid blocked labels `[M]/[A]/[M+A]` work and counts match.
5. Missing project homes appears when `proj_...` tag exists without a project file; `p` creates it.
6. `?` shows dispatch and commands execute.
