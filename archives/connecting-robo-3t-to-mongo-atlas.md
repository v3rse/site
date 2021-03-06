title: Connecting Robo 3T to MongoDB Atlas
tags: tips
date: 2018-09-26 10:16:40
---

> This little tip is to help anyone struggling with connecting to MongoDB Atlas like I did.

![nosql](http://geekandpoke.typepad.com/.a/6a00d8341d3df553ef0148c80ac6ef970c-800wi)

MongoDB Atlas provides a managed MongoDB database service for various web applications. From time to time you may want to see whats happening in your database whether in your stage or production environment. There are a number of options when it comes to MongoDB clients. Some are listed below:

- [Studio 3T](https://studio3t.com/)
- [MongoDB Compass](https://www.mongodb.com/products/compass)
- [Robo 3T](https://robomongo.org/) (formerly Robomongo)

The first is a piece of commercial software. Since we aim to be frugal I won't talk about it much. I do think you should give it a try. It is a great tool for database administrators. The last two are free. I started using Robo 3T in its Robomongo days when I'd just started using MongoDB so I'm more used to its interface. I used Compass a bit because that's what Atlas suggests you use but I didn't like the web-like interface (it felt a pit flakey).

The sad thing is that it is difficult to connect to Atlas using Robo 3T as opposed to Studio 3T and Compass. Both Compass and Studio 3T provide a feature that allows you to autofill the connection form fields using the connection link provided on Atlas. Alright enough talk let's get down to it.


#### Steps
> If you haven't downloaded Robo 3T please do. Also, I'm assuming you already have a MongoDB Atlas account.

1. Retrieve your connection link. Click the _Connect_ button.
![connect](/images/mongo-connect.png)

2. Create a new connection. Enter your the host details in Robo 3T.
![enter host](/images/mongo-enter-host.png)

3. Enter your credentials.
![enter credentials](/images/mongo-enter-credentials.png)

4. Setup SSL. I didn't need a pem cert/key in my case. All I had to do was select `Self-signed Certificate`.
![setup ssl](/images/mongo-setup-ssl.png)

5. Test the connection.
![test connection](/images/mongo-test-connection.png)

#### A side note
I had a little issue when setting mine up. In my case, after connecting, my databases weren't listing because of misconfigured permission. My work around here was to use the DB [shell commands](https://gist.github.com/amirkheirabadi/5859662) (since Robo 3T gives you an actually open a shell as opposed to Compass). I did the following to know my list of collections:

- Open a new shell
![open shell](/images/mongo-open-shell.png)
- Run `use <database-names>`
- List the collection using `show collections`

After that you can run any command you need. e.g.

```js
db.getCollection('todos').find({status: 'done'})
```

Hope this saves someone sometime.

__Happy Coding__