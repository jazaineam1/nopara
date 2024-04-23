library(car)
plot(prestige ~ income, xlab="Average Income",
     ylab="Prestige", data=Prestige, pch=19)
mod_lowess <- lowess(x=Prestige$income, y=Prestige$prestige, f=3/4)

plot(prestige ~ income, xlab="Average Income",
     ylab="Prestige", data=Prestige, pch=19)
lines(mod_lowess, lwd=4, col='tomato')

library(plotly)
plot_ly(x=Prestige$income,
        y=Prestige$education,
        z=Prestige$prestige, type="scatter3d", color=Prestige$prestige) %>%
    layout(scene = list(xaxis = list(title = 'Income'),
                        yaxis = list(title = 'Education'),
                        zaxis = list(title = 'Prestige')))

mod_loess <- loess(prestige ~ income + education, data=Prestige,
                   degree=2, span=0.75)


inc <- with(Prestige, seq(min(income), max(income), len=25))
edu <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=edu)
fit.prestige <- matrix(predict(mod_loess, newdata), 25, 25)
plot_ly(x=inc, y=edu, z=fit.prestige) %>% add_surface() %>%
    layout(scene = list(xaxis = list(title = 'Income'),
                        yaxis = list(title = 'Education'),
                        zaxis = list(title = 'Prestige')))
