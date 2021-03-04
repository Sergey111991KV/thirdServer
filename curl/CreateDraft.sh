#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d '{"tagsId":[1,2],"mainPhotoUrl":"main photo 1 news draft","newsIdDraft":1,"textDraft":"some text draft for 1 news","shortNameDraft":"draft1","otherPhotoUrl":["draft 1 photo","draft 2 photo"],"dataCreateDraft":"2011-09-18T20:00:00Z","idAuthorDraft":2,"idDraft":5}'  -X POST http://localhost:3000/draft  -H "Content-Type: application/json"


