package net.sf.birdy.help.core.model;

public class Article extends ModelImpl{

    @Override
    public Model findModel(String key) {
        if (this.getName().equals(key)){
            return this;
        }
        return null;
    }

}
