"makeEDMtree" <-
function()
{
tmp3 <- rpart(party ~ EDM29 + EDM52 + EDM89 + EDM110 + EDM125 + EDM142 + EDM175 + EDM297 + EDM346 + EDM391 + EDM445 + EDM476 + EDM493 + EDM516 + EDM657 + EDM770 + EDM939, data=mpincdf99)
tmp3
}
