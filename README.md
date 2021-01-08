# The News Server
    This is my third attempt to write a server)
    I used ReaderT design pattern.

## Configuration.
  All configuration are in the src/Config.hs and you need to write in your customization or if need I can rewrite project with change "server.config"
                log configuration ( logFile - where to write?, logLevelForFile - recording level(Debug, Warning ,Error), logConsole  - is write to console?
### start bot 
    All you need:
     create file "server.config" like file from folder templates
     replaced:
                "port" with your port
                "postgres" - with your postgres option
     take command: stack ghci 
          

## Structure of project.
###    All logic divided for 2 group: 
*   Server  (main logic) 
    *   Domain/Services - all logic for Server (Auth, Log, Common, Sorted, Filter)
    *   Domain/Types - all types(Auth, Log, Bisiness - Author,News... ) 
    *   Domain/Config - implementashion config
*   Adapter (implementation): 
    *   Adapter/PostgresSQL - implementashion http postgres
    *   Adapter/HTTP - implementashion http protocol
###    All module have 2 imports:
                ImportLibrary - import common method and librarys inside in file 
                Import*MODULE - import external all method of MODULE
            
           
## How to use.
        All you need to start project - create file "server.config" , write your option and script to start project) 
        Server can all method writen in rizoma. For check it you can run bash in folder /curl

## Test.
    I used spec library for testing app and empty structure.

## Api 
### Registration and Authentication

Registration and Authentication | Api
------------ | -------------
auth  | Get /api/auth/:login/:password  
exit  | Get /api/auth/exit             


### Get methods all entity

Entity       | Api
------------ | -------------
users        | GET /api/getAll/user
authors      | GET /api/getAll/author
tags         | GET /api/getAll/tag
categories   | GET api/getAll/category
drafts       | GET /api/getAll/draft
news         | GET /api/getAll/news

### Get methods one entity

Entity       | Api
------------ | -------------
users | GET /api/getOne/user/:id
authors | GET /api/getOne/author/:id
tags | GET /api/getOne/tag/:id
categories | GET /api/getOne/category/:id
drafts | GET /api/getOne/draft/:id
news | GET /api/getOne/news/:id
comments | GET /api/getOne/comment/:id

### Create methods

Entity       | Api
------------ | -------------
author | POST /api/create/author
tag | POST /api/create/tag
category | POST /api/create/category
draft | POST /api/create/draft
news by publishing draft | PUT /api/publish/:id
comment | POST /api/create/comment
user |  POST /api/create/user

### Edit methods


Entity       | Api
------------ | -------------
author | PUT /api/editing/author
tag | PUT /api/editing/tag 
category | PUT /api/editing/category
draft | PUT /api/editing/draft
user | PUT /api/editing/user

### Delete methods

Entity       | Api
------------ | -------------
user | DELETE /api/delete/author/:id
author | DELETE /api/delete/user/:id
tag | DELETE /api/delete/tag/:id
category | DELETE /api/delete/category/:id
draft | DELETE /api/delete/draft/:id
news | DELETE /api/delete/author/:id
comment | DELETE /api/delete/comment/:id

### Sorted news

Sort         | Api
------------ | -------------
author | /api/news/sortedNews/author
category | /api/news/sortedNews/category
date | /api/news/sortedNews/date
photo | /api/news/sortedNews/photo

### Filter news

Filter       | Api
------------ | -------------
all of Tag | /api/news/filterAllOfTags/:id,id
one of Tag | /api/news/filterOneOfTags/:id,id
tag | /api/news/filterTeg/:id
author | /api/news/filterAuthor/:id
category | /api/news/filterCategory/:id
content | /api/news/filterContent/:descrip
name | /api/news/filterName/:news
date | /api/news/filterOfData/less/:yyyy-mm-dd
