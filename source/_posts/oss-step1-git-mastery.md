title: 'A journey into Open Source [Step 1 - Git Mastery]'
tags: journal
date: 2016-04-21 03:20:03
---


Git: The intro
===

![Git](https://imgs.xkcd.com/comics/git.png)

I've been thinking about about getting into open source for the longest time. To get the procrastination out of the way I decided to blog about starting. And publish the blogs!!..oooh scary.

I'm guessing since I'm going to be working with GitHub a lot it's about time I faced this weird little monster called `git`.

Coming from an Java background I used `SVN`...a lot. It was simple enough to use at work and with an IDE you didn't even have to know the commands. But `git` has proven a bit more difficult to master mainly because I haven't used it as frequently as I may have wanted. Plus it doesn't seem as easy to understand as `SVN` mainly because:

> It is a distributed Version Control System (VCS)!!

Which means:
* You’ll have a __local repository__ which lives inside a special folder named `.git`. This you can commit changes even when offline.
* You’ll normally (but not necessarily) have a __remote, central repository__ where different collaborators may contribute their code.
* Each of those contributors has an __exact clone__ of the repository on their local workstation.

Types VCS
---
![Local version control](https://git-scm.com/book/en/v2/book/01-introduction/images/local.png)
*Local version control*

![Centralized version control](https://git-scm.com/book/en/v2/book/01-introduction/images/centralized.png)
*Centralized version control*

![Distributed version control.](https://git-scm.com/book/en/v2/book/01-introduction/images/distributed.png)
*Distributed version control*


Under The Hood
---
This is what `git` actually does. It manipulates files in your file system based on commands you give it. It manages everything it does logical as tree structure . The commands I mentioned help you navigate and manipulate the tree structure. One of these commands is the `commit` which creates a node in this tree.

Think of the `master` branch as container for/grouping of nodes(commits) on the tree with the most recent node on top. `HEAD`,on the other hand, is a **reference** to the **node** the `work space` of the repository currently points to.

![Tree](http://imgs.xkcd.com/comics/git_commit.png)


Getting My Hands Dirty
===

 __I have a project I want to be 'versioned' using `git`__.

Easy!
```
#Setup git
git config --global user.name "Nana Adane"
git config --global user.email "nanaofosuheneadane@gmail.com"

#Pop into the directory
cd git-test

#And run
git init

```

__What if someone gave me a link to a project I want to contribute to?__

```
#Clone the repository.
git clone <the-link>
#N/B:This gets all the history pertaining to this project
```

__I want to see what's happening with my repository__
```
#Check the status of the repository
git status
```
This prints out some helpful stuff about your repo like:
```
On branch master
nothing to commit, working directory clean
```

__I have a project. How do I add stuff?__

```
#Create a file
touch hello.txt
echo 'Hello,World' > hello.txt

#Add the file.
git add hello.txt

#Commit it
git commit -m 'Add new file'
#N/B: It's a common convention to use presence when writing commit messages
```

__I want to create a new feature in my project__

`Git` is highly optimized for branching and merging quickly. Branches aren't copies of the working directory folder. Instead they are just another grouping of nodes on the tree. `Git` keeps track of branch changes with the same folder as the working directory and makes changes to the working directory when new branch is checked out.

Conventionally new features to a project are created in a branch. Feature branches are then merged into the master branch after testing and then deleted. `Git` allows us to do this quickly and easily because now files are actually copied.

```
#Create new branch
git branch new-file-feature

#List current branches. The branch you're on is indicated by '*'
git branch

#Checkout/switch to the branch you created
git checkout new-file-feature

#Write some code and commit
touch new-file.txt
echo Starts out in only 'new-file-feature branch' > new-file.txt
git add new-file.txt
git commit -m "Add a new file"
```

__Oh no! I have conflict. How do I merge?__

A conflict occurs when two or more sets of changes are made to the same file. For `git` this usually happens when you attempt to merge changes from two branches that have changes to the same files. A merge copies nodes of the tree contained within a branch into another branch. Conflicts can be resolved manually by editing the file in conflict and committing it.

```
#Let's assume I also made a change to the original hello.txt file within the new branch create previously
#To merge changes to my master branch
git checkout master
git merge new-file-feature
```

You should get a message like

```
Auto-merging hello.txt
CONFLICT (content): Merge conflict in hello.txt
```

To resolve the conflict
```
#Open the file in an editor
vim hello.txt
```

You should see something like
```
Hello, world!
<<<<<<< HEAD
Hi I was changed in master
=======
Hi I was added in the new-file-feature branch
>>>>>>> new-feature-branch
```

Edit to look the way you want
```
Hello, world!
Hi I was changed in master
Hi I was added in the new-file-feature branch
```
N/B: Your conflicts may span multiple lines



__I want to go back in time and try some stuff__

`Git` keeps a log of all commits. It keeps SHA references to each commit. This idea for jumping back to previous commit.

```
#List all previous commits. Copy the reference
git log
#Hit the 'q' to exit

#Checkout the specific commit
git checkout 92225e88
```
This prints out a helpful message:

```
Note: checking out '92225e88'.

You are in 'detached HEAD' state. You can look around, make experimental
changes and commit them, and you can discard any commits you make in this
state without impacting any branches by performing another checkout.

If you want to create a new branch to retain commits you create, you may
do so (now or later) by using -b with the checkout command again. Example:

  git checkout -b new_branch_name

HEAD is now at 92225e8... Add line to hello.txt
```
A **detached HEAD state** basically means that the work space is currently not pointing to any container for commits (a branch). This means that if you checkout a branch you'll lose all commits made here.

```
Warning: you are leaving 1 commit behind, not connected to
any of your branches:

  51a579c Undid a change I made

If you want to keep them by creating a new branch, this may be a good time
to do so with:

 git branch new_branch_name 51a579c

Switched to branch 'master'
```

To save your commits you can create a new branch for them.

__I don't like the way the project looks now. How do I undo the changes?__

There are two ways of making this happen:

* `reset` is for changes that haven't been pushed to a remote repository yet.

```
git reset --hard 92225e88
```

Doing this on pushed changes will cause the shared history of the project to change.

* `revert` is for changes that have been pushed to a remote server.

```
git revert 92225e88
```
This creates a commit that removes all changes in the specified commit.

__Finally I'm done with my changes. Let's share it with the world__

We've speaking of a remote repository. After making all your feature changes you may want to share it with the rest of your team.

You will need a `remote` for that.

```
#'origin' is the name you give to your remote
git remote add origin git@github.com:v3rse/hallo.git
```

If you cloned the repository then you already have a remote.

```
#You can list your remotes
git remote -v
```

N/B: A remote may even be a cloud server(maybe a Quality Assurance server or a Continuous Integration server) which you can deploy to by pushing.

```
#You can push to a specific branch on remote
git push origin master
```

Shortcuts
---
* Add and Committing
```
#Add and commit a file already added once
git commit -a -m "<some message>"
```

* Branching and Checking out
```
#Branching and checking out in one command
git checkout -b <new-branch-name>
```

I'll be writing later on the differences between `fetch` and `push` later as well as the mysteries `rebase` and  **squash**.

GUI Tools
---
* GitK Tool
This is a minimal tool to help you to visually understand your repository

```
#Install gitk
sudo apt-get install gitk
#show all commits reachable from any reference not just HEAD
gitk --all
```

![Gitk](https://git-scm.com/book/en/v2/book/A-git-in-other-environments/images/gitk.png)



* Ungit
This one tool I used often in the past. It's good for all beginners because of the visuals.

```
#Install ungit
npm install -g ungit
#show start it up in your browser
cd project-folder
ungit
```

![Ungit](https://github.com/FredrikNoren/ungit/raw/master/screenshot.png)


References
===
* [Git Explained: For Beginners](https://dzone.com/articles/intro-git)
* [Getting Started-About Version Control](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)
* [Git GUIs](https://git-scm.com/book/en/v2/Git-in-Other-Environments-Graphical-Interfaces)

__Happy Coding__
