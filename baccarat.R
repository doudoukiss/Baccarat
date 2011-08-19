## Baccarat is a card game,played at casinos and by recreational gamblers.

bac <- function(deck = 8, res = 52){    # deck is the total number of decks of the cards, res is how many cards should be left when the game ends

    if(res < 6) stop("res should be bigger than 6 !")

      x = sample(c(rep(c(1:9,rep(0,4)),4*deck)),52*deck)   # shuffle up the cards

      mat = NULL

      k = 1   # the kth card

      up.lim = 52*deck - res

      while(k < up.lim){

            player = (x[k] + x[k+2]) %% 10

                banker = (x[k+1]+x[k+3]) %% 10

                if(player > 7 || banker >7){

                        mat = rbind(mat,c(player,banker))   # Score matrix

                              k = k + 4

                      }else

                if(player > 5){

                        if(banker < 6){

                                  banker = (banker + x[k+4]) %% 10

                                          k = k + 5 } else

                              k = k + 4

                              mat = rbind(mat,c(player,banker))

                      }else{

                              player = (player + x[k+4]) %% 10

                                    if(x[k+4]==2 || x[k+4]==3){   # If the Player drew a 2 or 3, the Banker draws if he has 0?4, and stands if he has 5?7.

                                              if(banker < 5){

                                                          banker = (banker + x[k+5]) %% 10

                                                                    k = k + 6 } else

                                                      k = k + 5

                                            }else

                                    if(x[k+4]==4 || x[k+4]==5){   # If the Player drew a 4 or 5, the Banker draws if he has 0?5, and stands if he has 6?7.

                                              if(banker < 6){

                                                          banker = (banker + x[k+5]) %% 10

                                                                    k = k +6 } else

                                                      k= k + 5

                                            } else

                                    if(x[k+4]==6 || x[k+4]==7){   # If the Player drew a 6 or 7, the Banker draws if he has 0?6, and stands if he has 7.

                                              if(banker < 7){

                                                          banker = (banker + x[k+5]) %% 10

                                                                    k = k + 6 } else

                                                      k = k + 5

                                            } else

                                    if(x[k+4] == 8){   # If the Player drew an 8, the Banker draws if he has 0?2, and stands if he has 3?7.

                                              if(banker < 3){

                                                          banker = (banker + x[k+5]) %% 10

                                                                    k = k + 6 } else

                                                      k = k + 5

                                            } else

                                    if(banker < 4){   # If the Player drew an ace, 9, 10, or face-card, the Banker draws if he has 0?3, and stands if he has 4?7.

                                              banker = (banker + x[k+5]) %% 10

                                                      k = k + 6 } else

                                    k = k + 5

                                    mat = rbind(mat,c(player,banker))

                            }

          }

      colnames(mat) = c("player","banker")

      return(mat)

  }
