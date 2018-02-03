fs = require('fs')
request = require('request')
mongoskin = require('mongoskin')
mkdirp = require('mkdirp')
path = require('path')
async = require('async')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}").open( (err, _) -> throw err if err )

same = (key, t) ->
  return [key, t]

rename = (newkey) ->
  return (key, t) -> [newkey, t]

setFields = {
  "name" : same
  "code" : same
  "format" : same
  "position" : same
}

mapSets = {}

cardFields = {
  "Set" : rename("setname"),
  "Primary" : rename("type"),
  "Alignment" : rename("side"),
  "MEID" : same,
  "Artist" : same,
  "Rarity" : same,
  "Precise" : same,
  "NameEN" : rename("title"),
  "NameFR" : same,
  "NameGR" : same,
  "NameSP" : same,
  "NameJP" : same,
  "ImageName" : same,
  "Text" : rename("text"),
  "Skill" : rename("subtype"),
  "MPs" : same,
  "Mind" : same,
  "Direct" : same,
  "General" : same,
  "Prowess" : same,
  "Body" : same,
  "Corruption" : same,
  "Home" : same,
  "Unique" : (k, t) -> ["uniquness", if t is null then 0 else 1],
  "Secondary" : same,
  "Race" : same,
  "RWMPs" : same,
  "Site" : same,
  "Path" : same,
  "Region" : same,
  "RPath" : same,
  "Playable" : same,
  "GoldRing" : same,
  "GreaterItem" : same,
  "MajorItem" : same,
  "MinorItem" : same,
  "Information" : same,
  "Palantiri" : same,
  "Scroll" : same,
  "Haven" : same,
  "Stage" : same,
  "Strikes" : same,
  "code" : same,
  "codeFR" : same,
  "codeGR" : same,
  "codeSP" : same,
  "codeJP" : same,
  "Specific" : same,
  "fullCode" : same,
  "alignCode" : same,
  "setCode" : same,
  "normalizedtitle" : same
}

baseurl = "http://192.168.1.180:8080/rez/"

selectFields = (fields, objectList) ->
  ((Object.keys(fields).reduce ((newObj, key) ->
    newKey = fields[key](key, obj[key]) if typeof(obj[key]) isnt "undefined"
    newObj[newKey[0]] = newKey[1] if newKey and newKey[1] isnt null
    return newObj), {}) \
  for obj in objectList)

fetchSets = (callback) ->
  request.get baseurl + "sets", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body)
      sets = selectFields(setFields, data)
      for set in sets
        mapSets[set.code] = set
        imgDir = path.join(__dirname, "..", "resources", "public", "img", "cards", "#{set.code}")
        mkdirp imgDir, (err) ->
          if err
            console.error("Failed to create card image resource directory #{imgDir}")
      db.collection("sets").remove ->
        db.collection("sets").insert sets, (err, result) ->
          fs.writeFile "andb-sets.json", JSON.stringify(sets), ->
            console.log("#{sets.length} sets fetched")
          callback(null, sets.length)

fetchImg = (urlTemplate, card, localPath, t) ->
  setTimeout ->
    console.log("Downloading image for " + card)
    url = urlTemplate + code
    request(url).pipe(fs.createWriteStream(localPath))
  , t

fetchCards = (callback) ->
  request.get baseurl + "cards", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body)
      cards = selectFields(cardFields, data)
      imgDir = path.join(__dirname, "..", "resources", "public", "img", "cards")
      i = 0
      for card in cards
        imgPath = path.join(imgDir, "#{card.setname}", "#{card.ImageName}")
        if !fs.existsSync(imgPath)
          fetchImg((baseurl + card.setname + "/"), card.ImageName, imgPath, i++ * 200)
      db.collection("cards").remove ->
        db.collection("cards").insert cards, (err, result) ->
          fs.writeFile "andb-cards.json", JSON.stringify(cards), ->
            console.log("#{cards.length} cards fetched")
            callback(null, cards.length)

async.series [fetchSets, fetchCards, () -> db.close()]
