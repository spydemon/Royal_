package net.sf.royal.gui.util.graph;

import net.sf.royal.gui.manager.LocaleManager;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;

public class LoanDetailGraph {

    
    private DefaultCategoryDataset dataset = new DefaultCategoryDataset();
    private ChartPanel chartPanel;
    
    public LoanDetailGraph(){
        
        dataset.addValue(0.0, "", GraphFactory.jan);
        dataset.addValue(0.0, "", GraphFactory.feb);
        dataset.addValue(0.0, "", GraphFactory.mar);
        dataset.addValue(0.0, "", GraphFactory.avr);
        dataset.addValue(0.0, "", GraphFactory.may);
        dataset.addValue(0.0, "", GraphFactory.jun);
        dataset.addValue(0.0, "", GraphFactory.jul);
        dataset.addValue(0.0, "", GraphFactory.aug);
        dataset.addValue(0.0, "", GraphFactory.sep);
        dataset.addValue(0.0, "", GraphFactory.oct);
        dataset.addValue(0.0, "", GraphFactory. nov);
        dataset.addValue(0.0, "", GraphFactory.dec);
        
        JFreeChart chart = ChartFactory.createBarChart(
                "",         // chart title
                "",               // domain axis label
                "",                  // range axis label
                dataset,                  // data
                PlotOrientation.VERTICAL, // orientation
                false,                     // include legend
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
    
    public void setValues(int[] values){
        dataset.setValue(values[0], "", GraphFactory.jan);
        dataset.setValue(values[1], "", GraphFactory.feb);
        dataset.setValue(values[2], "", GraphFactory.mar);
        dataset.setValue(values[3], "", GraphFactory.avr);
        dataset.setValue(values[4], "", GraphFactory.may);
        dataset.setValue(values[5], "", GraphFactory.jun);
        dataset.setValue(values[6], "", GraphFactory.jul);
        dataset.setValue(values[7], "", GraphFactory.aug);
        dataset.setValue(values[8], "", GraphFactory.sep);
        dataset.setValue(values[9], "", GraphFactory.oct);
        dataset.setValue(values[10], "", GraphFactory.nov);
        dataset.setValue(values[11], "", GraphFactory.dec);
    }

    public void clear() {
        dataset.setValue(0.0, "", GraphFactory.jan);
        dataset.setValue(0.0, "", GraphFactory.feb);
        dataset.setValue(0.0, "", GraphFactory.mar);
        dataset.setValue(0.0, "", GraphFactory.avr);
        dataset.setValue(0.0, "", GraphFactory.may);
        dataset.setValue(0.0, "", GraphFactory.jun);
        dataset.setValue(0.0, "", GraphFactory.jul);
        dataset.setValue(0.0, "", GraphFactory.aug);
        dataset.setValue(0.0, "", GraphFactory.sep);
        dataset.setValue(0.0, "", GraphFactory.oct);
        dataset.setValue(0.0, "", GraphFactory. nov);
        dataset.setValue(0.0, "", GraphFactory.dec);
    }
}
