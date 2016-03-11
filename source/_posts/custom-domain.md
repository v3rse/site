title: Github Pages with a Custom subdomain name - Hexo Style
date: 2016-03-11 04:20:26
tags: snippet
---

This is going to be quick.

So you don't like the ___username_.github.com__.

You want to change that to __blog.superrat.com__ or something weird like that

Here's how make it happen:

- Follow the steps [here](https://help.github.com/articles/setting-up-your-pages-site-repository/#adding-your-custom-domain-to-a-cname-file) to create a CNAME file in your site's repo
- Go to your domain name vendor's account page and create a CNAME record. This may vary for different vendors. I use [namecheap.com](www.namecheap.com) so I followed [this guide](https://www.namecheap.com/support/knowledgebase/article.aspx/9646/10/how-can-i-set-up-a-cname-record-for-my-domain).
- Now this is the tricky part. Create a CNAME file in _yourblog_/.deploy\_git and _yourblog_/public directories identical to the one you've created in the repo. This will prevent hexo from  deleting the CNAME file in the remote repo due to conflicts. Update this file locally and then __hexo deploy__ when you want to use a different domain.

There you have it. Now your blog should be available in your weird custom domain name now. Give me some feedback in the comments.

__I'll post a tutorial on how a blog these days later for newbies to Hexo or static site generation.__

__Happy Coding__
