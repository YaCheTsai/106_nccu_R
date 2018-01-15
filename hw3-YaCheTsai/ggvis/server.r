  shinyServer(function(input, output) { 
  output$distPlot <- renderPlot({
    if (input$slider1[1]!=input$slider1[2]) {
         data(iris)
        # log transform 
        log.ir <- log(iris[, input$slider1[1]:input$slider1[2]])
        ir.species <- iris[, 5]
        # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
        ir.pca <- prcomp(log.ir,
                         center = TRUE, 
                         scale. = TRUE)
        print(ir.pca)
        library(ggbiplot)
        g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                      groups = ir.species, ellipse = TRUE, 
                      circle = TRUE)
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal', 
                       legend.position = 'top')
        print(g)
    } 
    
    })
    
    output$distPlot2 <- renderPlot({
    if (input$slider1[1]!=input$slider1[2]) {
         data(iris)
        # log transform 
        log.ir <- log(iris[, input$slider1[1]:input$slider1[2]])
        ir.species <- iris[, 5]
        # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
        ir.pca <- prcomp(log.ir,
                         center = TRUE, 
                         scale. = TRUE)
        plot(ir.pca, type = "l")
    } 
    
    })
    
    output$summary <- renderPrint({
    if (input$slider1[1]!=input$slider1[2]) {
         data(iris)
        # log transform 
        log.ir <- log(iris[, input$slider1[1]:input$slider1[2]])
        ir.species <- iris[, 5]
        # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
        ir.pca <- prcomp(log.ir,
                         center = TRUE, 
                         scale. = TRUE)
        summary(ir.pca)
    } 
    })
    output$view <- renderTable({
    if (input$slider1[1]!=input$slider1[2]) {
         data(iris)
        # log transform 
        log.ir <- log(iris[, input$slider1[1]:input$slider1[2]])
        ir.species <- iris[, 5]
        # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
        ir.pca <- prcomp(log.ir,
                         center = TRUE, 
                         scale. = TRUE)
        head(predict(ir.pca, newdata=tail(log.ir, 2)), n = 20)
    } 
    
    })

    
    })