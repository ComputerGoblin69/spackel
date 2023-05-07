pub trait Stack {
    type Item: Copy;
    type Error;

    fn push(&mut self, element: Self::Item);

    fn pop(&mut self) -> Result<Self::Item, Self::Error>;

    fn dup(&mut self) -> Result<(), Self::Error> {
        let v = self.pop()?;
        self.push(v);
        self.push(v);
        Ok(())
    }

    fn swap(&mut self) -> Result<(), Self::Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(b);
        self.push(a);
        Ok(())
    }

    fn over(&mut self) -> Result<(), Self::Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a);
        self.push(b);
        self.push(a);
        Ok(())
    }

    fn nip(&mut self) -> Result<(), Self::Error> {
        let b = self.pop()?;
        self.pop()?;
        self.push(b);
        Ok(())
    }

    fn tuck(&mut self) -> Result<(), Self::Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(b);
        self.push(a);
        self.push(b);
        Ok(())
    }
}
