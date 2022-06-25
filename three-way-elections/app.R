library(tidyverse)
library(shiny)
library(thematic)
library(bslib)

on22a <- read.csv("ontario2022.csv",header=TRUE)

gridlines <- list(
  annotate("text",label="PC",x=0.5,y=32.5,hjust=0,size=6,color="blue"),
  annotate("text",label="NDP",x=-0.5,y=32.5,hjust=1,size=6,color="orange"),
  annotate("text",label="PC",x=16.3*sqrt(3),y=-13.3,hjust=0,size=6,color="blue"),
  annotate("text",label="Lib",x=16.3*sqrt(3),y=-17.3,hjust=0,size=6,color="red"),
  annotate("text",label="NDP",x=-16.3*sqrt(3)+2,y=-13.3,hjust=1,size=6,color="orange"),
  annotate("text",label="Lib",x=-16.3*sqrt(3)+2,y=-17.3,hjust=1,size=6,color="red"),
  #vertical
  annotate("segment",x=15*sqrt(3),xend=15*sqrt(3),y=-15,yend=17,size=.5),
  annotate("segment",x=10*sqrt(3),xend=10*sqrt(3),y=-10,yend=22,size=.3),
  annotate("segment",x=5*sqrt(3),xend=5*sqrt(3),y=-5,yend=27,size=.3),
  annotate("segment",x=0,xend=0,y=0,yend=32,arrow=arrow(),size=1),
  annotate("segment",x=-5*sqrt(3),xend=-5*sqrt(3),y=-5,yend=27,size=.3),
  annotate("segment",x=-10*sqrt(3),xend=-10*sqrt(3),y=-10,yend=22,size=.3),
  annotate("segment",x=-15*sqrt(3),xend=-15*sqrt(3),y=-15,yend=17,size=.5),
  #down-right (parallel to LIB-CON)
  annotate("segment",x=0,xend=16*sqrt(3),y=30,yend=30-16,size=0.5),
  annotate("segment",x=0,xend=16*sqrt(3),y=20,yend=20-16,size=0.3),
  annotate("segment",x=0,xend=16*sqrt(3),y=10,yend=10-16,size=0.3),
  annotate("segment",x=0,xend=16*sqrt(3),y=0,yend=-16,arrow=arrow(),size=1),
  annotate("segment",x=-5*sqrt(3),xend=11*sqrt(3),y=-5,yend=-5-16,size=0.3),
  annotate("segment",x=-10*sqrt(3),xend=6*sqrt(3),y=-10,yend=-10-16,size=0.3),
  annotate("segment",x=-15*sqrt(3),xend=sqrt(3),y=-15,yend=-15-16,size=0.5),
  #down-left (parallel to NDP-LIB)
  annotate("segment",x=0,xend=-16*sqrt(3),y=30,yend=30-16,size=0.5),
  annotate("segment",x=0,xend=-16*sqrt(3),y=20,yend=20-16,size=0.3),
  annotate("segment",x=0,xend=-16*sqrt(3),y=10,yend=10-16,size=0.3),
  annotate("segment",x=0,xend=-16*sqrt(3),y=0,yend=-16,arrow=arrow(),size=1),
  annotate("segment",x=5*sqrt(3),xend=-11*sqrt(3),y=-5,yend=-5-16,size=0.3),
  annotate("segment",x=10*sqrt(3),xend=-6*sqrt(3),y=-10,yend=-10-16,size=0.3),
  annotate("segment",x=15*sqrt(3),xend=-sqrt(3),y=-15,yend=-15-16,size=0.5),
  theme_void(),
  coord_fixed()
)

#These figures are how much ground each party has lost since 2018. Scraped from 338.
lib18 <- -8.13
ndp18 <- 10.59
pcp18 <- 4.1
x0 = -(pcp18-lib18)/2*sqrt(3)+(ndp18-lib18)/2*sqrt(3)
y0 = -(pcp18-lib18)/2-(ndp18-lib18)/2

ui <- fluidPage(theme = bslib::bs_theme(bg = "#D4E0F7",
                                        fg = "#000000",
                                        base_font = font_google("Fira Sans")),
                titlePanel("Depicting the Three-Way Race in Ontario"),
                p(tags$h4(em("Note: This was meant to be about polling in advance of the election, and thus does not mention the results of the election itself.
                             I might add the results later, but the three-way graphic is interesting enough on its own (I hope!)."))),
                selectInput("controller", "Choose:",
                            choices=c("Latest Projections"="fournier",
                                      "A Uniform Swing Model"="uniform",
                                      "Voting Strategically"="strategic"),
                            selected="fournier"),
                tabsetPanel(id = "on_tabs",
                            type = "hidden",
                            tabPanelBody("fournier",
                                         p(tags$b("This graphic tries to depict how *close* each riding is likely to be in Ontario, and which parties are competitive where."),
                                           "Where there are two competitive parties, this is an easy task: the higher the vote for one party, the lower it is for the other party, and you can make a list from most favourable to least favourable."),
                                         p("In a three-party system it's more complicated. In many urban ridings only the Liberals and NDP are competitive, while in most 905 ridings only the Liberals and Conservatives are competitive.
                                        A party might gain ground compared to one party while losing ground to the other.
                                          This graph tries to be a useful way to show everything at once."),
                                         p("You can hover over a dot to see which riding it is and what the projected figures are. (You might have to click inside the graph first.)"),
                                         plotOutput("fournier", height="600px", hover="hover1"),
                                         tableOutput("data1"),
                                         br(),
                                         p(em("The graphic might look three-dimensional but it is not — none of the points are floating.
                                             It can only handle three parties, so we ignore the Greens.
                                             The data is from Philippe J. Fournier's ",a(href="https://338canada.com/ontario/","338Canada model")," from May 25.")),
                                         p(tags$b("This graph can also help explain what 'vote efficiency' is. "),"Notice that the NDP has a bunch of districts deep into its 'territory', while most of the Liberals have many ridings along their 'boundaries'.
                                          With the difference of a few points in the polls, the Liberals can go from losing all of these ridings (and then you'll hear about how 'inefficient' their vote was), to winning all of them (and then their vote will suddenly be 'efficient'!)."),
                                         p(em(tags$small("(I had scraped a more recent version but my computer lost it somehow! And I can't scrape it again, as the site has replaced the projections with the final results.)"))),
                                         br(),
                                         hr(),
                                         p(tags$small("Developed by Koji Shiromoto ( @kojisposts |", a(href="https://kojisposts.wordpress.com","Website"),")"),style="text-align:center")),
                            tabPanelBody("uniform",
                                         p("One possible use of this diagram is to show which districts might flip first the next time around: districts that were won with the narrowest margins are expected to be the first to be lost when fortunes turn the other way.
                                          Australian election-watchers make ", em(a(href="https://www.abc.net.au/news/elections/federal/2022/guide/pendulum","'Mackerras pendulums'")), " that order districts according to their margins: the stronger a party's performance, the further down their opponent's list of ridings it can conquer.
                                          BBC election broadcasts usually show ",em(a(href="https://www.bbc.com/news/av/election-2017-39979450","'Swingometers'")),", which are similar in idea."),
                                         p(tags$b("So you can use this as a 'three-way swingometer.'"),"
                                           You can put the 2018 results on the diagram, and move the position of the axes depending on how the overall polls have moved.
                                          If the Liberals have gained 5 points and the PCs lost 5 points, then the Liberal-PC line moves further into PC territory, and blue districts start being flipped red.
                                          This diagram does exactly that: I put the 2018 results on the diagram and moved the axes based on how the Ontario-wide polls have shifted.
                                          (The grey axes are from the 2018 election, and the axes shifted according to the arrow to the black axes.)
                                          The districts in between the old axes and the new ones are the ones that can be expected to flip."),
                                         plotOutput("uniform", height="600px", hover="hover2"),
                                         tableOutput("data2"),
                                         br(),
                                         p("This 'uniform swing method' is, to be sure, a really simple way of predicting elections —", tags$b("so simple that it is not very accurate."),
                                           "It presumes that, if the Liberals gain 5 points in Ontario and the PCs lose 5, then the Liberals have gained exactly 5 points (and the PCs lost exactly 5 points) in ",tags$b("every single riding"),", regardless of local factors.
                                          In reality, different types of voters react differently to events, and Ontario-wide polling just averages them out."),
                                         p("Brainiacs who predict elections put a lot more variables into their models: local demographics, whether an incumbent is running this time or ran last time, riding polls, etc.
                                          You can see how the 338Canada model differs from a basic uniform-swing model here — you can tell there are some important differences!"),
                                         plotOutput("comparing", height="600px", hover="hover3"),
                                         tableOutput("data3"),
                                         br(),
                                         hr(),
                                         p(tags$small("Developed by Koji Shiromoto ( @kojisposts |", a(href="https://kojisposts.wordpress.com","Website"),")"),style="text-align:center")),
                            tabPanelBody("strategic",
                                         p("By depicting which parties are competitive in which ridings, the graph can also help you see how to vote strategically.
                                          In ridings along the bottom-right axis (the Liberal-PC axis), those seeking to defeat the PCs should vote Liberal; along the vertical axis (the NDP-PC axis), they should vote NDP. These ridings are marked red and orange, respectively.
                                          For ridings marked purple, the PCs are not competitive, but they could either flip Liberal or NDP, so you should vote accordingly."),
                                         plotOutput("strategic", height="600px", hover="hover4"),
                                         tableOutput("data4"),
                                         br(),
                                         hr(),
                                         p(tags$small("Developed by Koji Shiromoto ( @kojisposts |", a(href="https://kojisposts.wordpress.com","Website"),")"),style="text-align:center"))),
                padding=20)


server <- function(input, output, session){
  thematic::thematic_shiny(font="auto")
  observeEvent(input$controller, {
    updateTabsetPanel(session, "on_tabs", selected=input$controller)
  })
  output$fournier <- renderPlot({
    ggplot(on22a, aes(x=x2,y=y2)) +
      gridlines +
      geom_point(aes(color=win_proj,shape=likely)) +
      scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red","Toss up"="grey50")) + 
      theme(legend.position = "bottom",
            text = element_text(family="Fira Sans"),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15))
  })
  output$data1 <- renderTable({
    nearPoints(on22a[,c(2,9:11,13:14,20:21)], input$hover1,xvar="x2",yvar="y2")
  })
  output$uniform <- renderPlot({
    ggplot(on22a, aes(x=x0,y=y0)) +
      gridlines +
      annotate("segment",x=x0,y=y0,xend=x0,yend=32-x0/sqrt(3),color="grey50",size=1) +
      annotate("segment",x=x0,y=y0,xend=16*sqrt(3)-(pcp18-lib18)/2*sqrt(3),yend=-16-(pcp18-lib18)/2,color="grey50",size=1) +
      annotate("segment",x=x0,y=y0,xend=-16*sqrt(3)+(ndp18-lib18)/2*sqrt(3),yend=-16-(ndp18-lib18)/2,color="grey50",size=1) +
      annotate("segment",x=x0-.5,y=y0+.5,xend=.5,yend=-.5,arrow=arrow(),color="grey50",size=2) +
      annotate("label",x=-6*sqrt(3),y=-12,color="grey50",label="NDP->Lib\nflip",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
      annotate("label",x=9*sqrt(3),y=-13,color="grey50",label="PC->Lib\nflip",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
      annotate("text",x=3,y=12,color="grey50",label="PC->\nNDP\nflip",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
      geom_point(aes(color=win18)) +
      scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red")) + 
      theme(legend.position = "bottom",
            text = element_text(family="Fira Sans"),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15))
  })
  output$data2 <- renderTable({
    nearPoints(on22a[,c(2:8,18:19)], input$hover2,xvar="x0",yvar="y0")
  })
  output$comparing <- renderPlot({
    ggplot(on22a) +
      gridlines +
      geom_point(aes(x=x2,y=y2,color=region)) +
      geom_segment(aes(x=x0,y=y0,xend=x2,yend=y2,color=region),alpha=0.3) + 
      theme(legend.position = "bottom",
            text = element_text(family="Fira Sans"),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15))
  })
  output$data3 <- renderTable({
    nearPoints(on22a[,c(2,6:11,20:21)], input$hover3,xvar="x2",yvar="y2")
  })
  output$strategic <- renderPlot({
    ggplot(on22a, aes(x=x2,y=y2)) +
      gridlines +
      geom_point(aes(color=strat,shape=likely)) +
      scale_color_manual(values=c("NDP"="orange","LIB"="red","Green"="green",
                                  "Lib or NDP"="purple","Doesn't matter"="grey50")) + 
      theme(legend.position = "bottom",
            text = element_text(family="Fira Sans"),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15))
  })
  output$data4 <- renderTable({
    nearPoints(on22a[,c(2,9:11,13:14,20:21)], input$hover4,xvar="x2",yvar="y2")
  })
}

shinyApp(ui = ui, server = server)