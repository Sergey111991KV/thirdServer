# The News Server

This is my third attempt to write a server)
I used ReaderT design pattern.

## Configuration.
All configuration are in the src/Config.hs and you need to write in your customization or if need I can rewrite project with change "server.config":

            log configuration ( logFile - where to write?, logLevelForFile - recording level(Debug, Warning ,Error),
            logConsole  - is write to console?
## Start server 
All you need:
            
            create file "server.config" (like file from folder templates)
Replaced:

            "port" with your port
            "postgres" - with your postgres option
Take command:

            stack ghci 
          

## Structure of project.
###    All logic divided for 2 group: 
*   Server  (main logic) 
    *   Domain/Services - all logic for Server (Auth, Log, Common, Sorted, Filter)
    *   Domain/Types - all types(Auth, Log, Bisiness - Author,News... ) 
    *   Domain/Config - implementashion config
*   Adapter (implementation): 
    *   Adapter/PostgresSQL - implementashion http postgres
    *   Adapter/HTTPWAI - implementashion http protocol
###    All module have 2 imports:
                ImportLibrary - import common method and librarys inside in file 
                Export*MODULE - import external all method of MODULE
            
           
## How to use.
All you need to start project - create file "server.config" , write your option and script to start project) 
Server can all method writen in rizoma. For check it you can run bash in folder /curl

## Test.
I used spec library for testing app and empty structure.

## Api 
### Registration and Authentication

Registration and Authentication | Api
------------ | -------------
auth  | Get /auth/:login/:password  
exit  | Get /auth/exit             


### Get standard methods all entity

Entity       | Api
------------ | -------------
users        | GET /users
authors      | GET /authors
tags         | GET /tags
categories   | GET categories
drafts       | GET /drafts
news         | GET /news_s


Query Param:  

Param        | Keys  
------------ | ---- 
page (Pagination parameter)    | Int   


### Get methods one entity

Entity       | Api
------------ | -------------
users | GET /user
authors | GET /author
tags | GET /tag
categories | GET /category
drafts | GET /draft
news | GET /news
comments | GET /comment

Query Param:  

Param        | Keys  
------------ | ---- 
id_Entity    | Int        

### Create methods

Entity       | Api
------------ | -------------
author | POST /author
tag | POST /tag
category | POST /category
draft | POST /draft
comment | POST /create/comment
user |  POST /create/user


### Publish methods (it unites in itself Create and Edit methods of news)

PUT /publish/:id

### Edit methods

Entity       | Api
------------ | -------------
author | PUT /author
tag | PUT /tag 
category | PUT /category
draft | PUT /draft
user | PUT /user

### Delete methods

Entity       | Api
------------ | -------------
user | DELETE /author/:id
author | DELETE /user/:id
tag | DELETE /tag/:id
category | DELETE /category/:id
draft | DELETE /draft/:id
news | DELETE /author/:id
comment | DELETE /comment/:id

### Sorted news

GET /sortedNews

Query Param:  

Param        | Keys | Param       | Keys 
------------ | ---- | ----------- | ---- 
page (Pagination parameter) | Int
conditionSorted | date | conditionOfDate |  ASC or DESC :: Text
conditionSorted | author 
conditionSorted | category 
conditionSorted | photo 
           

### Filter news


GET /filterNews

Query Param: 

Param        | Keys | Param       | Keys | Param       | Keys 
------------ | ---- | ----------- | ---- | ----------- | -----
page (Pagination parameter) | Int
filterCondition | date |  condition |   ASC or DESC :: Text | date | Text
filterCondition | author |  author_id |  Int
filterCondition | category |  category_id |  Int
filterCondition | tag |  tag_id |   Int
filterCondition | oneOfTag |   tags_arr (Сondition one of Tag Array Id)  |   Text
filterCondition | allOfTag |   tags_arr (Сondition all of Tag Array Id)  | Text 
filterCondition | name |  name_filter |   Text
filterCondition | content |  content |  Text
 

                    
              