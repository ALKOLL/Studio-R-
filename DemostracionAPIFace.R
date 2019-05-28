library(httr)
library(jpeg)
library(reshape2)
library(imager)
baseUrl <- "https://westcentralus.api.cognitive.microsoft.com/face/v1.0/detect"
q <- "?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,headPose,facialHair,glasses,emotion"
url1 <- paste(baseUrl, q, sep="")
 

img1Url <- "http://vis-www.cs.umass.edu/lfw/images/Diego_Diego_Lerman/Diego_Diego_Lerman_0001.jpg"
img2Url <- "http://vis-www.cs.umass.edu/lfw/images/Jack_Nicholson/Jack_Nicholson_0003.jpg"


f1<- tempfile()
download.file(img1Url, f1, mode="wb")
pic1 <- upload_file(f1)

f2<- tempfile()
download.file(img2Url, f2, mode="wb")
pic2 <- upload_file(f2)

#Envia peticion Microsoft Face API
response = POST(url=url1, body=pic1, add_headers(.headers = 
                c('Content-Type'='application/octet-stream', 'Ocp-Apim-Subscription-Key'='31749446adbf4a6eabcc4599c8931e48')))

result <- content(response)
df <- as.data.frame(result)
df2 <- melt (df, id = c ("faceId"))

# plot the image
img1 <- readJPEG(f1)
plot(0:1, 0:1, xlim=range(c(0,150)), ylim=range(c(0,150)),type='n', asp=1)
rasterImage(img1, 0, 0, 150, 150, interpolate=F)

# tip of the nose
tip_nose_x = df[ ,c("faceLandmarks.noseTip.x")]
tip_nose_y = df[ ,c("faceLandmarks.noseTip.y")]

IMAGE_HEIGHT <- 150

#now let's draw the dot at that location
points(tip_nose_x, IMAGE_HEIGHT - tip_nose_y, pch=19, col="green")

# localizacion pupila izq
left_pupil_x = df[ ,c("faceLandmarks.pupilLeft.x")]
left_pupil_y = df[ ,c("faceLandmarks.pupilLeft.y")]
points(left_pupil_x, IMAGE_HEIGHT - left_pupil_y, pch=19, col="green")

#  localizacion pupila der
right_pupil_x = df[ ,c("faceLandmarks.pupilRight.x")]
right_pupil_y = df[ ,c("faceLandmarks.pupilRight.y")]
points(right_pupil_x, IMAGE_HEIGHT - right_pupil_y, pch=19, col="green")

# rectangulo de la cara
xleft <- df[ ,c("faceRectangle.left")]
ytop <- IMAGE_HEIGHT - df[ ,c("faceRectangle.top")]
ybottom <- ytop - df[ ,c("faceRectangle.height")]
xright <- df[ ,c("faceRectangle.left")] + df[ ,c("faceRectangle.width")]
rect(xleft, ybottom, xright, ytop, col=NA, border="magenta", lwd=2)

# localizacion pupila izq
left_Mouth_x = df[ ,c("faceLandmarks.mouthLeft.x")]
left_Mouth_y = df[ ,c("faceLandmarks.mouthLeft.y")]
points(left_Mouth_x, IMAGE_HEIGHT - left_Mouth_y, pch=19, col="red")

#  localizacion pupila der
right_Mouth_x = df[ ,c("faceLandmarks.mouthRight.x")]
right_Mouth_y = df[ ,c("faceLandmarks.mouthRight.y")]
points(right_Mouth_x, IMAGE_HEIGHT - right_Mouth_y, pch=19, col="red")

#Envia peticion Microsoft Face API de la segunda fotografia
response = POST(url=url1, body=pic2, add_headers(.headers = 
  c('Content-Type'='application/octet-stream', 'Ocp-Apim-Subscription-Key'='efad5a9c25514506a47f1f4efe406e0a')))

result2 <- content(response)
dfSecondPicture <- as.data.frame(result2)
dfSecondPicturePivoted <- melt (dfSecondPicture, id = c ("faceId"))

# Se genera solicitud para la comparacion de las 2 fotografias
pic1FaceId <- df[,c("faceId")]
pic2FaceId <- dfSecondPicture[ ,c("faceId")]

baseUrlFindSimilar <- "https://westcentralus.api.cognitive.microsoft.com/face/v1.0/findsimilars"
bodyFindSimilar <- sprintf('{"faceId": "%s",
                           "faceIds": ["%s", "%s"],
                           "mode": "%s"}',
                           pic1FaceId,
                           pic1FaceId,
                           pic2FaceId,
                           "matchFace")

#Se envia solicitud para la comparacion de las dos fotografias a Microsoft Face API - Find Similar
responseSimilar = POST(url=baseUrlFindSimilar, body=bodyFindSimilar, add_headers(.headers = 
  c('Content-Type'='application/json', 'Ocp-Apim-Subscription-Key'='efad5a9c25514506a47f1f4efe406e0a')))

resultSimilar <- content(responseSimilar)
dfSimilar <- as.data.frame(resultSimilar)

cat("\n", "Similitud entre la primera foto y si misma(Porcentaje): ", dfSimilar[,c("confidence")])
cat("\n", "Similitud entre las 2 fotos(Porcentaje): ", dfSimilar[,c("confidence.1")])

# cleanup
file.remove(f1)
file.remove(f2)
