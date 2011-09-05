package net.sf.royal.gui.wizard;

import java.io.File;

import net.sf.royal.gui.filechooser.ImageChooser;

public class ImageWizardModel extends WizardModel {

    private File selectedFile;
    
    public String getNextPage() {
        this.getWizardView().setButtonState(WizardView.FINISH);
        return ImageWizard.KEY_FC;
    }

    public String getPreviousPage() {
        // Do nothing
        return null;
    }

    public boolean finish() {
        selectedFile = ((ImageChooser) this.getWizardView().getWizardPage(ImageWizard.KEY_FC).getPanel().getComponent(0)).getSelectedFile();
        return Boolean.TRUE.booleanValue();
    }

    /**
     * @return Returns the selectedFile.
     */
    public File getSelectedFile() {
        return selectedFile;
    }
   

}
