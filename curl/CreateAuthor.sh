#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d '{"idAuthor":3,"idLinkUser":{"userIdRaw":3},"description":"Oleg author"}'  -X POST http://localhost:3000/author  -H "Content-Type: application/json"


