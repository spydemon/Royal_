package net.sf.royal.gui.util.graph;

import net.sf.royal.gui.manager.LocaleManager;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;

public class LoanDaysGraph {

    private DefaultCategoryDataset dataset = new DefaultCategoryDataset();
    private ChartPanel chartPanel;
    String serieMax = null;
    String serieEff = null;
    
    public LoanDaysGraph() {
        
        serieMax = LocaleManager.getInstance().getString("graph_max_days");
        serieEff = LocaleManager.getInstance().getString("garph_really_days");
        
        dataset.addValue(0.0, serieEff, GraphFactory.jan);
        dataset.addValue(0.0, serieEff, GraphFactory.feb);
        dataset.addValue(0.0, serieEff, GraphFactory.mar);
        dataset.addValue(0.0, serieEff, GraphFactory.avr);
        dataset.addValue(0.0, serieEff, GraphFactory.may);
        dataset.addValue(0.0, serieEff, GraphFactory.jun);
        dataset.addValue(0.0, serieEff, GraphFactory.jul);
        dataset.addValue(0.0, serieEff, GraphFactory.aug);
        dataset.addValue(0.0, serieEff, GraphFactory.sep);
        dataset.addValue(0.0, serieEff, GraphFactory.oct);
        dataset.addValue(0.0, serieEff, GraphFactory. nov);
        dataset.addValue(0.0, serieEff, GraphFactory.dec);
        
        dataset.addValue(0.0, serieMax, GraphFactory.jan);
        dataset.addValue(0.0, serieMax, GraphFactory.feb);
        dataset.addValue(0.0, serieMax, GraphFactory.mar);
        dataset.addValue(0.0, serieMax, GraphFactory.avr);
        dataset.addValue(0.0, serieMax, GraphFactory.may);
        dataset.addValue(0.0, serieMax, GraphFactory.jun);
        dataset.addValue(0.0, serieMax, GraphFactory.jul);
        dataset.addValue(0.0, serieMax, GraphFactory.aug);
        dataset.addValue(0.0, serieMax, GraphFactory.sep);
        dataset.addValue(0.0, serieMax, GraphFactory.oct);
        dataset.addValue(0.0, serieMax, GraphFactory. nov);
        dataset.addValue(0.0, serieMax, GraphFactory.dec);
        
        JFreeChart chart = ChartFactory.createAreaChart(
                "",         // chart title
                "",               // domain axis label
                "",                  // range axis label
                dataset,                  // data
                PlotOrientation.VERTICAL, // orientation
                true,                     // include legend
                false,                     // tooltips?
                false                     // URLs?
            );
        
        CategoryPlot plot = chart.getCategoryPlot();
        final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
        rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        
        plot.setForegroundAlpha(0.6f);

        
        chartPanel = new ChartPanel(chart);
        chartPanel.setLocale(LocaleManager.getInstance().getCurrentLocale());
    }
    
    public ChartPanel getChartPanel() {
        return chartPanel;
    }
    
    public void setValues(double[] maxDays, double[] effDays){
        dataset.setValue(maxDays[0], serieMax, GraphFactory.jan);
        dataset.setValue(maxDays[1], serieMax, GraphFactory.feb);
        dataset.setValue(maxDays[2], serieMax, GraphFactory.mar);
        dataset.setValue(maxDays[3], serieMax, GraphFactory.avr);
        dataset.setValue(maxDays[4], serieMax, GraphFactory.may);
        dataset.setValue(maxDays[5], serieMax, GraphFactory.jun);
        dataset.setValue(maxDays[6], serieMax, GraphFactory.jul);
        dataset.setValue(maxDays[7], serieMax, GraphFactory.aug);
        dataset.setValue(maxDays[8], serieMax, GraphFactory.sep);
        dataset.setValue(maxDays[9], serieMax, GraphFactory.oct);
        dataset.setValue(maxDays[10], serieMax, GraphFactory. nov);
        dataset.setValue(maxDays[11], serieMax, GraphFactory.dec);
        
        dataset.setValue(effDays[0], serieEff, GraphFactory.jan);
        dataset.setValue(effDays[1], serieEff, GraphFactory.feb);
        dataset.setValue(effDays[2], serieEff, GraphFactory.mar);
        dataset.setValue(effDays[3], serieEff, GraphFactory.avr);
        dataset.setValue(effDays[4], serieEff, GraphFactory.may);
        dataset.setValue(effDays[5], serieEff, GraphFactory.jun);
        dataset.setValue(effDays[6], serieEff, GraphFactory.jul);
        dataset.setValue(effDays[7], serieEff, GraphFactory.aug);
        dataset.setValue(effDays[8], serieEff, GraphFactory.sep);
        dataset.setValue(effDays[9], serieEff, GraphFactory.oct);
        dataset.setValue(effDays[10], serieEff, GraphFactory. nov);
        dataset.setValue(effDays[11], serieEff, GraphFactory.dec);
    }

    public void clear() {
        dataset.addValue(0.0, serieMax, GraphFactory.jan);
        dataset.addValue(0.0, serieMax, GraphFactory.feb);
        dataset.addValue(0.0, serieMax, GraphFactory.mar);
        dataset.addValue(0.0, serieMax, GraphFactory.avr);
        dataset.addValue(0.0, serieMax, GraphFactory.may);
        dataset.addValue(0.0, serieMax, GraphFactory.jun);
        dataset.addValue(0.0, serieMax, GraphFactory.jul);
        dataset.addValue(0.0, serieMax, GraphFactory.aug);
        dataset.addValue(0.0, serieMax, GraphFactory.sep);
        dataset.addValue(0.0, serieMax, GraphFactory.oct);
        dataset.addValue(0.0, serieMax, GraphFactory. nov);
        dataset.addValue(0.0, serieMax, GraphFactory.dec);
        
        dataset.addValue(0.0, serieEff, GraphFactory.jan);
        dataset.addValue(0.0, serieEff, GraphFactory.feb);
        dataset.addValue(0.0, serieEff, GraphFactory.mar);
        dataset.addValue(0.0, serieEff, GraphFactory.avr);
        dataset.addValue(0.0, serieEff, GraphFactory.may);
        dataset.addValue(0.0, serieEff, GraphFactory.jun);
        dataset.addValue(0.0, serieEff, GraphFactory.jul);
        dataset.addValue(0.0, serieEff, GraphFactory.aug);
        dataset.addValue(0.0, serieEff, GraphFactory.sep);
        dataset.addValue(0.0, serieEff, GraphFactory.oct);
        dataset.addValue(0.0, serieEff, GraphFactory. nov);
        dataset.addValue(0.0, serieEff, GraphFactory.dec);
    }
}
