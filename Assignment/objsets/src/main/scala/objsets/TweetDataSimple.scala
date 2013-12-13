package objsets

// real tweet data, collected on Oct 1
object TweetDataSimple {
  val gizmodo = """[
{ "user": "gizmodo", "text": "Kindle Paperwhite Review: Forget Everything Else, This Is the E-Reader You Want http://t.co/737W6aNC", "retweets": 51.0 },
{ "user": "gizmodo", "text": "These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp", "retweets": 49.0 },
{ "user": "gizmodo", "text": "Ever wonder why the sky is dark at night? Here's your answer. http://t.co/eTKxkcaE", "retweets": 86.0 },
{ "user": "gizmodo", "text": "The head of Homeland Security stays secure by just not using email, at all. http://t.co/W6KAFEUu", "retweets": 37.0 },
{ "user": "gizmodo", "text": "This is how graphene will grow the flexible semiconductors of the future: http://t.co/IoEvuxp4", "retweets": 43.0 },
{ "user": "gizmodo", "text": "It's the tech-based reality TV show you never knew you didn't want: http://t.co/j9J8gAo8", "retweets": 19.0 },
{ "user": "gizmodo", "text": "How do you make your Steve Jobs sculpture stand out? Easy, mix in some trash you stole from him. http://t.co/mvHBj3CH", "retweets": 15.0 },
{ "user": "gizmodo", "text": "This awesome baggage roller coaster will make you wish you were a suitcase. http://t.co/ECaE2hgd", "retweets": 17.0 },
{ "user": "gizmodo", "text": "This price cut is putting the Kindle Paperwhite and the Nook with GlowLight head to head. Fight! http://t.co/1x6nOJGY", "retweets": 15.0 },
{ "user": "gizmodo", "text": "18 unlucky people who already broke the iPhone 5: http://t.co/9RpvX4te", "retweets": 79.0 },
{ "user": "gizmodo", "text": "Here's how to ditch cash and start paying for everything electronically. http://t.co/tgrZ50XM", "retweets": 30.0 },
{ "user": "gizmodo", "text": "If you're going to get a Nexus 7, you should just go out and buy it right now. http://t.co/tACY1YYG", "retweets": 33.0 },
{ "user": "gizmodo", "text": "The one thing you can buy that will make your smartphone better: http://t.co/rlA32n4r", "retweets": 74.0 },
{ "user": "gizmodo", "text": "Cold brew Irish coffee! It's the best part of waking up (drunk) http://t.co/cr3oCPKf", "retweets": 23.0 },
{ "user": "gizmodo", "text": "Maybe the best reminder yet that the coffee shop is not your office: http://t.co/T8aNd3SX", "retweets": 47.0 },
{ "user": "gizmodo", "text": "Why a brand new, billion-dollar battleship still needs old-timey wooden ladders: http://t.co/75yOeche", "retweets": 13.0 },
{ "user": "gizmodo", "text": "Time for another story bundle! Seven crime novels, pay what you want, support indie authors! http://t.co/kI7tFMvM", "retweets": 14.0 },
{ "user": "gizmodo", "text": "Apple's added a Maps section to its App Store to help you find an Apple Maps replacement: http://t.co/rpP0tCMI", "retweets": 114.0 },
{ "user": "gizmodo", "text": "Man jailed for accidentally sexting his whole address book (including some tweens) http://t.co/5IxaTHkz #WTFriday", "retweets": 65.0 },
{ "user": "gizmodo", "text": "An Iranian news agency thought an Onion article was real--and plagiarized it: http://t.co/C4lWe1Ij", "retweets": 112.0 },
{ "user": "gizmodo", "text": "Yep, the TSA is definitely stealing iPads: http://t.co/THYd8MKe", "retweets": 79.0 },
{ "user": "gizmodo", "text": "A week with the iPhone 5: http://t.co/ReuK1aJs", "retweets": 111.0 },
{ "user": "gizmodo", "text": "Tim Cook apologizes for Apple Maps and suggests you download... Bing: http://t.co/imQFFOyW", "retweets": 280.0 },
{ "user": "gizmodo", "text": "Neil Young unveils his new Pono music player: http://t.co/EEMznUio", "retweets": 27.0 },
{ "user": "gizmodo", "text": "This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE", "retweets": 101.0 },
{ "user": "gizmodo", "text": "NASA has found proof of a river on Mars: http://t.co/QPPUSQXp", "retweets": 242.0 },
{ "user": "gizmodo", "text": "14 people who should be ashamed of their @foursquare mayorship http://t.co/7AEHQJLT", "retweets": 50.0 },
{ "user": "gizmodo", "text": "This team of quadrocopter drones can throw and catch better than you http://t.co/GYdhzZMq", "retweets": 32.0 },
{ "user": "gizmodo", "text": "Steve Jobs' reality distortion field lives on http://t.co/CDnZi4z8", "retweets": 56.0 },
{ "user": "gizmodo", "text": "What's the worst children's toy you've ever seen? Let's bask in the awful http://t.co/fk41tJj0", "retweets": 18.0 },
{ "user": "gizmodo", "text": "Spies like us: why we're all Big Brother now http://t.co/MB3gXB0b", "retweets": 8.0 },
{ "user": "gizmodo", "text": "18 places carbon fiber just doesn't belong: http://t.co/vJo1Yhlj", "retweets": 21.0 },
{ "user": "gizmodo", "text": "A woman had a new ear grown on her arm and attached to her head and it is PRETTY GROSS: http://t.co/NyuikEmP", "retweets": 88.0 },
{ "user": "gizmodo", "text": "That iron man found by Nazis? It's of extraterrestrial origin. Yes, really. http://t.co/hRR7oufa", "retweets": 92.0 },
{ "user": "gizmodo", "text": "The new best desk toy since... maybe ever? http://t.co/IwuHYYgI", "retweets": 64.0 },
{ "user": "gizmodo", "text": "Poop snow on ancient burial ground. This can't end well. http://t.co/DAmSgIiu", "retweets": 30.0 },
{ "user": "gizmodo", "text": "Giant electronic circuits make wonderfully geeky art: http://t.co/um99UW4Y", "retweets": 20.0 },
{ "user": "gizmodo", "text": "This little robot will help make all that wine you drink: http://t.co/a6W5wY0p", "retweets": 16.0 },
{ "user": "gizmodo", "text": "Facebook's finally deleting thousands of face accounts: http://t.co/3quTouWv", "retweets": 51.0 },
{ "user": "gizmodo", "text": "These people actually really, really love Apple Maps: http://t.co/VvWsa5V7", "retweets": 29.0 },
{ "user": "gizmodo", "text": "Is your iPhone 5 camera seeing purple? Like, where it shouldn't be? http://t.co/EBnaMfFR", "retweets": 37.0 },
{ "user": "gizmodo", "text": "Ooh, a galaxy-shooting camera you might actually afford http://t.co/VLXkarGV", "retweets": 22.0 },
{ "user": "gizmodo", "text": "Is this the most stunning shuttle flyover ever? http://t.co/IYKbjmnx", "retweets": 59.0 },
{ "user": "gizmodo", "text": "Why Apple really ditched Google Maps http://t.co/evVBDYCu", "retweets": 104.0 },
{ "user": "gizmodo", "text": "Why is this part of Facebook broken? http://t.co/Y9Ur2U5T", "retweets": 12.0 },
{ "user": "gizmodo", "text": "Don't buy cheap iPhone 5 cables because they don't actually exist yet http://t.co/3LHLeCdO", "retweets": 56.0 },
{ "user": "gizmodo", "text": "How vicious spyware contaminated hundreds of thousands of rental PCs: http://t.co/MlrUDWZe", "retweets": 12.0 },
{ "user": "gizmodo", "text": "How Jack Daniels hand crafts every barrel for aging http://t.co/QeYKZDNv", "retweets": 54.0 },
{ "user": "gizmodo", "text": "Is putting pictures of missing children on 404 pages effective? http://t.co/WFh9tiZU", "retweets": 26.0 },
{ "user": "gizmodo", "text": "The gigantic crane that builds even more gigantic aircraft carriers http://t.co/KraKD1sE", "retweets": 17.0 },
{ "user": "gizmodo", "text": "The 20 most obvious PINs are painfully obvious http://t.co/Du9BYjeR", "retweets": 38.0 },
{ "user": "gizmodo", "text": "This is the closest, clearest view of Mars yet http://t.co/VrbZRVgR", "retweets": 45.0 },
{ "user": "gizmodo", "text": "Meet the new Apple, where things don't just work http://t.co/XS36kHWi", "retweets": 146.0 },
{ "user": "gizmodo", "text": "Cheap goodies on Google Play for the next five days: http://t.co/dT3bRBzZ", "retweets": 52.0 },
{ "user": "gizmodo", "text": "Do phones need to come with chargers these days? http://t.co/AZMsPUeD", "retweets": 21.0 },
{ "user": "gizmodo", "text": "The font on your car's dash might increase your risk of crashing: http://t.co/R6e6QiWp", "retweets": 27.0 },
{ "user": "gizmodo", "text": "Holy crap, self-driving cars are now legal in California! http://t.co/nSOwBSZ1", "retweets": 277.0 },
{ "user": "gizmodo", "text": "Judging by this photo, joining the USAF makes you a superhero http://t.co/XlCb7nK8", "retweets": 16.0 },
{ "user": "gizmodo", "text": "Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", "retweets": 290.0 },
{ "user": "gizmodo", "text": "Nikon D600 review: Images this spectacular have never been so cheap http://t.co/PK4LCOwX", "retweets": 33.0 },
{ "user": "gizmodo", "text": "Oh good, Iran's homemade war drone is in the air http://t.co/ySXchFfv", "retweets": 40.0 },
{ "user": "gizmodo", "text": "BTW, this is @brentrose tweeting for Gizmodo this week. Follow for witticisms and euphemisms.", "retweets": 0.0 },
{ "user": "gizmodo", "text": "Major Samsung security bug can wipe your Galaxy phone (updating) http://t.co/n5yDZ3dh", "retweets": 120.0 },
{ "user": "gizmodo", "text": "Aboard the ligher, smarter, deadlier aircraft carrier of tomorrow http://t.co/xVjQAr6J", "retweets": 22.0 },
{ "user": "gizmodo", "text": "NASA publishes the most detailed photo of the universe ever captured, and it's stunning http://t.co/68pDKByp", "retweets": 192.0 },
{ "user": "gizmodo", "text": "How NASA prevents a space plague outbreak http://t.co/jIeI7hPX", "retweets": 20.0 },
{ "user": "gizmodo", "text": "iPhone 5's brain dissected. Guess what, it's made by Samsung. http://t.co/wSyjvpDc", "retweets": 321.0 },
{ "user": "gizmodo", "text": "BlackBerry 10 adds so many features that so few people want http://t.co/eaQM0vlS", "retweets": 58.0 },
{ "user": "gizmodo", "text": "The supercomputer that houses the entire universe http://t.co/KQciouL2", "retweets": 47.0 },
{ "user": "gizmodo", "text": "Gambling website refunds everyone's Packers bets because of horrible refs http://t.co/bwmgYBDq", "retweets": 121.0 },
{ "user": "gizmodo", "text": "12 deadly inventions that killed their creators (not for the faint of heart) http://t.co/BCwRAzhe", "retweets": 82.0 },
{ "user": "gizmodo", "text": "You won't believe this amazing this wildlife photography contest winner http://t.co/nHIRgFVG", "retweets": 20.0 },
{ "user": "gizmodo", "text": "How to build an iPhone 5 dock for $1.27 http://t.co/kqsQ1GIV", "retweets": 65.0 },
{ "user": "gizmodo", "text": "Would you eat this disgusting stuff? (Spoiler: yes, you would) http://t.co/mRAmDRpk", "retweets": 25.0 },
{ "user": "gizmodo", "text": "The iPhone 5 'shortage' is apparently a result of its new ultra-thin display: http://t.co/RqUSuYif", "retweets": 48.0 },
{ "user": "gizmodo", "text": "How much do you care about how Twitter measures your influence? http://t.co/0fitjfCD", "retweets": 19.0 },
{ "user": "gizmodo", "text": "Eric Schmidt confirms a Google Maps app on iOS 6 is still some way off: http://t.co/bobRuY06", "retweets": 76.0 },
{ "user": "gizmodo", "text": "Breville YouBrew coffee maker review: excellent, expensive, safe for idiots (like @harrysawyers) http://t.co/67wKNuLM", "retweets": 11.0 },
{ "user": "gizmodo", "text": "Can this flashy redesign make Myspace cool again? http://t.co/OfHVPHz6", "retweets": 94.0 },
{ "user": "gizmodo", "text": "No, your private Facebook messages have not gone public. Filthy secrets, still safe. http://t.co/RAJsAyK0", "retweets": 67.0 },
{ "user": "gizmodo", "text": "iOS 5.1 for Apple TV brings new AirPlay goodness http://t.co/1Yj55T52", "retweets": 48.0 },
{ "user": "gizmodo", "text": "The Facebook for poor people you didn't know existed http://t.co/9zmMTZci", "retweets": 29.0 },
{ "user": "gizmodo", "text": "(In case you're wondering who the awesome speller is, that's @brentrose. He has an MFA.)", "retweets": 2.0 },
{ "user": "gizmodo", "text": "iPhone 5 vs Galaxy S III: Who's screen is prettier? http://t.co/n6CbaspY", "retweets": 108.0 },
{ "user": "gizmodo", "text": "Yeah, we live in the future, but there's all this awesome 50-year-old Jetsons tech we still don't have http://t.co/lXsqu49Z", "retweets": 46.0 },
{ "user": "gizmodo", "text": "This tiny telescope implant gives eyesight to the blind http://t.co/W50RHP4L", "retweets": 40.0 },
{ "user": "gizmodo", "text": "Iran shuts down Google, will completely cut citizens fff the internet http://t.co/60ZucBic", "retweets": 146.0 },
{ "user": "gizmodo", "text": "10 stupid, crazy, wonderful gadget fails http://t.co/p3Al28G5", "retweets": 32.0 },
{ "user": "gizmodo", "text": "Watch an NBA player dunk the iPhone 5. Boomshakalaka! http://t.co/UQAX0awf", "retweets": 47.0 },
{ "user": "gizmodo", "text": "Is your iPhone 5... rattling? http://t.co/mn0r2dhb", "retweets": 67.0 },
{ "user": "gizmodo", "text": "The definitive comparison of iOS 5 Google Maps vs iOS 6 Apple Maps in one single image: http://t.co/fTwTfVMy", "retweets": 191.0 },
{ "user": "gizmodo", "text": "Study splits breast cancer into four treatable types: http://t.co/myTn3LHu", "retweets": 23.0 },
{ "user": "gizmodo", "text": "IBM's supercomputer genius Watson is headed for the cloud: http://t.co/Z1psttXB", "retweets": 59.0 },
{ "user": "gizmodo", "text": "Nuance is planning to develop voice recognition which works while your phone sleeps. Good idea? http://t.co/WEXD9bQa", "retweets": 25.0 },
{ "user": "gizmodo", "text": "Is your new iPhone picking up more scratches than you'd like? http://t.co/DGEiawOi", "retweets": 35.0 },
{ "user": "gizmodo", "text": "Foxconn has shut down a factory after 2,000 of its employees started a massive brawl. http://t.co/d7TXo3K9", "retweets": 90.0 },
{ "user": "gizmodo", "text": "If you thought the NES Zapper was cool before, watch it set things on fire with a laser. http://t.co/t3f0oE8O", "retweets": 37.0 },
{ "user": "gizmodo", "text": "Want to hack NFC to get free train rides? There's an app for that. http://t.co/eAp6yTbE", "retweets": 82.0 },
{ "user": "gizmodo", "text": "The weirdest thing people hate about the iPhone 5: http://t.co/GMwuRp8D", "retweets": 202.0 },
{ "user": "gizmodo", "text": "Data centers waste a completely absurd amount of energy. http://t.co/NcO9pXqb", "retweets": 58.0 }
]"""
  val TechCrunch = """[
{ "user": "TechCrunch", "text": "Resignation Media Hires CEO John Ellis To Run Tapiture, Its Fast-Growing Pinterest For Men  http://t.co/ctn7oWJc by @anthonyha", "retweets": 18.0 }
]"""
  val engadget = """[
{ "user": "engadget", "text": "Sony reveals Japan prices for Windows 8 VAIO machines -  http://t.co/FRCu2XVb", "retweets": 18.0 },
{ "user": "engadget", "text": "FreedomPop's pay-as-you-go data service launches in beta, offering 500MB of free WiMAX per month -  http://t.co/Ny48yXUl", "retweets": 10.0 }
]"""
  val amazondeals = """[
{ "user": "amazondeals", "text": "Deal of the Day: 50% Off Select BEARPAW Boots &amp; Slippers http://t.co/ZBvpSN33", "retweets": 0.0 },
{ "user": "amazondeals", "text": "Lightning Deal! $38.69 - Honeywell Portable Wireless Door Chime and Push Button http://t.co/yYBV4ebr", "retweets": 0.0 }
]"""
  val CNET = """[
{ "user": "CNET", "text": "HP aims for business users with ElitePad 900 Windows 8 tablet http://t.co/XyOipAdL", "retweets": 10.0 },
{ "user": "CNET", "text": "RT @CNETNews: Apple rolls out fix for iPhone 5 Wi-Fi network data bug http://t.co/Gz13JfDD", "retweets": 93.0 }
]"""
  val gadgetlab = """[
{ "user": "gadgetlab", "text": "A Clean, Well-Lighted Face: The Kindle Paperwhite http://t.co/WlUPzZbS by @strngwys", "retweets": 11.0 },
{ "user": "gadgetlab", "text": "Maps? Purple halos in photos? Screen glitches? What? iPhone 5 'problems' explained http://t.co/xVGBVhGE by @redgirlsays", "retweets": 22.0 },
{ "user": "gadgetlab", "text": "Tim Cook apologizes for Mapocalypse debacle, offers interim fixes http://t.co/ahPbLcU5 by @redgirlsays", "retweets": 18.0 }
]"""
  val mashable = """[
{ "user": "mashable", "text": "Which is sorriest, #Apple Maps or #Siri? @Charlie_White has the comic - http://t.co/YRXXJXGY", "retweets": 55.0 },
{ "user": "mashable", "text": "Watch These Quadrotor #Robots Learn to Play Catch [VIDEO] http://t.co/CHZ5BPfL", "retweets": 21.0 },
{ "user": "mashable", "text": "10 Funny @YouTube Clips of Sleeping Dogs http://t.co/ukPgWZjH", "retweets": 34.0 }
]"""
}
