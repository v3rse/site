title: Github Pages with a Custom subdomain name - Hexo Style
date: 2016-03-11 04:20:26
tags: snippet
---

This is going to be quick.

So you don't like the ```username.github.com```.

You want to change that to ```blog.superrat.com``` or something weird like that

Here's how make it happen:

- Follow the steps [here](https://github.com/leecrossley/hexo-generator-cname) to auto generate a CNAME file in your site's repo
- Go to your domain name vendor's account page and create a CNAME record. This may vary for different vendors. I use [namecheap.com](www.namecheap.com) so I followed [this guide](https://www.namecheap.com/support/knowledgebase/article.aspx/9646/10/how-can-i-set-up-a-cname-record-for-my-domain).
- Now this is the tricky part. The url in your ```_config.yml``` should match the sub domain you created above.

There you have it. Now your blog should be available using your weird custom domain name now. Give me some feedback in the comments.

__I'll post a tutorial on how I blog these days for newbies to Hexo and static site generation.__

__Happy Coding__
