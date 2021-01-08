#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d '{"idAuthor":0,"idLinkUser":{"userIdRaw":1},"description":"Oleg author"}'  -X POST http://localhost:3000/api/create/author  -H «Content-Type:application/json» 


