title: A journey into Open Source • Step 2 - GitHub Guru
date: 2016-04-22 07:35:34
tags: journal
---

{% blockquote %}
This is an entry in my code journal. I'm currently writing on the open source software world. This is mainly so that I can Google my thoughts later and even share the experience with others who plan on getting in on the OSS action. The first part was on {% post_link oss-step1-git-mastery `git` mastery %}. These entries will be constantly evolve as my knowledge grows so stay tuned. **This next part is on working with GitHub.**
{% endblockquote %}

## GitHub: The intro

For those who didn't know `git` wasn't made by [GitHub](https://github.com/) (believe me I've heard that before). It was made by [Linus Torvalds](https://en.wikipedia.org/wiki/Linus_Torvalds), the same awesome guy that wrote the Linux kernel.

Github is actually a service that provides hosting (amongst other things) for `git` based projects. It's actually where this blog is [hosted](https://github.com/v3rse/v3rse.github.io)!! Remember the remote repository we were talking about last time? Well Github gives you a free way to host and manage your remote repository but there's a catch.

It's only free when your repository is public i.e. everybody gets to see your crappy/awesome code and copy it. There's a paid option for organizations that want to keep their repositories private. There are other popular alternatives to GitHub like [Bitbucket](https://bitbucket.org) and [GitLab](https://gitlab.com/) which give you **free private repositories** and many other cool features.

### Under The Hood

GitHub is pretty simple to use and it has lots of [good documentation](https://help.github.com/) on how to use it.

I'll just focus on explaining how it is used for open source projects. Let's say you find a good project you want to contribute to. Maybe this [one](https://github.com/DreamOval/iwallet-node-connector) (Shameless plug I know). Let's have a look at how Github makes it easy for you to contribute to such a project.

  ![project page](/images/Selection_034.png)

* Code:
  This is the first tab you see when you hit the project page. It displays the contents of README.md file if it's present. Other things you may see are general `git` statistics just below the project description eg. number of commit, number of branches etc.

  ![git stats](/images/git_stats.png)

  It also shows you a file manager view of the source code of the project. You can navigate and view the code by clicking the links to the folders and files. Notice that the last commit message for each folder and file is displayed to the right.

  ![git stats](/images/source.png)

  The most important section on this tab (IMHO) is this one.

  ![pull request bar](/images/pullbar.png)

  From here you can get the project link in case you want to `clone` it. You also get to create **Pull Requests** which I'll talk about later.

* Issue:
  GitHub gives you an **issue tracker** for your projects. An 'issue' may be a bug, a feature or even sometimes just a discussion on a particular concept used within the project.

  The issue tab usually looks like this:

  ![issue tab](/images/issue_tab.png)

  This is where you should look when you want to add some value to an open source project you like.

  Each issue may have a community given **label** that tells you what it's about. These labels also help you filter and search quickly through issues of larger projects.

  ![issue item](/images/issue_item.png)

  When an issue is `opened` you can discuss its resolution with people who raised the issue and with the rest of the community. This is how quite a number of people contribute to open source projects!

  ![issue page](/images/issue_page.png)

  After an issue is considered resolved it is `closed`.

* Pull requests:
  This I feel is the 'killer app' of GitHub. Every code contribution you make is put into a pull request. You aren't allowed to directly commit changes to a project you don't own.

  When you see an issue you want to resolve, the first thing you may have to do is `fork` the repository. This is more or less `branching` on a higher level.

  ![fork](/images/fork.png)

  Your `fork` allows you `clone` the project, create a feature branch, write your code and then commit you changes(since you own this `fork`).

  After writing all that awesome code you have to create a pull request for your changes(ideally they should be small testable units of code). This allows the maintainer or the community to review your code and make a decision on whether it is ready to be merged into their project. Some communities have CI servers that run your code at this stage to make sure every thing works well.

  ![pull request](/images/pull_request.png)

  This may sound intimidating but depending on the community this can actually be fun. You may get good coding advice and different ideas on how to improve your code. You can even make it a habit to review other people's code to learn new and amazing techniques.

  **N/B: most projects have rules and conventions governing how to contribute code and write issues. Please check out the Wiki tab or CONTRIBUTING.md file before writing any code or opening an issue**

* Wiki:
  This is where some projects keep their documentation and rules on how to contribute and comment. Be sure to check it out.

  ![wiki](/images/wiki.png)

  **N/B: Some projects actually have their docs in the code and not in the wiki. Like Bootstrap and Wordpress Calypso**

* Pulse:
  This tab gives you more detailed `git` statistics on the project as well as list the number of pull request merged, number of code authors, opened and closed issues etc.

  ![pulse](/images/pulse.png)

* Graphs:
  This gives you a more visual look into the project. Here you may see who contributes most to projects you like.

  ![graphs](/images/graphs.png)


Github has many other cool features that I haven't covered but feel free to explore, ask questions in the comments section and **get your hands dirty**. Speaking of which; since this post seems really long, all handle the `Getting My Hands Dirty` section in another post. I'll be setting up my environment for an actual [project](https://github.com/Automattic/wp-calypso).

See you in the next post.

__Happy Coding__
