<?php
/**
 * FilterSex
 */

namespace geneweb\api\filters;

class FilterSex extends Filter {

    protected $sex;

    public function __construct($sex = null){
        $this->setSex($sex);
    }

    public function setSex($sex)
    {
        $this->sex = $sex;
    }

    public function getSex()
    {
        return $this->sex;
    }
}
?>
